{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetRepositoryTriggers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about triggers configured for a repository.
--
--
module Network.AWS.CodeCommit.GetRepositoryTriggers
    (
    -- * Creating a Request
      getRepositoryTriggers
    , GetRepositoryTriggers
    -- * Request Lenses
    , grtRepositoryName

    -- * Destructuring the Response
    , getRepositoryTriggersResponse
    , GetRepositoryTriggersResponse
    -- * Response Lenses
    , grtrsConfigurationId
    , grtrsTriggers
    , grtrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get repository triggers operation.
--
--
--
-- /See:/ 'getRepositoryTriggers' smart constructor.
newtype GetRepositoryTriggers = GetRepositoryTriggers'
  { _grtRepositoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRepositoryTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grtRepositoryName' - The name of the repository for which the trigger is configured.
getRepositoryTriggers
    :: Text -- ^ 'grtRepositoryName'
    -> GetRepositoryTriggers
getRepositoryTriggers pRepositoryName_ =
  GetRepositoryTriggers' {_grtRepositoryName = pRepositoryName_}


-- | The name of the repository for which the trigger is configured.
grtRepositoryName :: Lens' GetRepositoryTriggers Text
grtRepositoryName = lens _grtRepositoryName (\ s a -> s{_grtRepositoryName = a})

instance AWSRequest GetRepositoryTriggers where
        type Rs GetRepositoryTriggers =
             GetRepositoryTriggersResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetRepositoryTriggersResponse' <$>
                   (x .?> "configurationId") <*>
                     (x .?> "triggers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRepositoryTriggers where

instance NFData GetRepositoryTriggers where

instance ToHeaders GetRepositoryTriggers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetRepositoryTriggers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRepositoryTriggers where
        toJSON GetRepositoryTriggers'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _grtRepositoryName)])

instance ToPath GetRepositoryTriggers where
        toPath = const "/"

instance ToQuery GetRepositoryTriggers where
        toQuery = const mempty

-- | Represents the output of a get repository triggers operation.
--
--
--
-- /See:/ 'getRepositoryTriggersResponse' smart constructor.
data GetRepositoryTriggersResponse = GetRepositoryTriggersResponse'
  { _grtrsConfigurationId :: !(Maybe Text)
  , _grtrsTriggers        :: !(Maybe [RepositoryTrigger])
  , _grtrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRepositoryTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grtrsConfigurationId' - The system-generated unique ID for the trigger.
--
-- * 'grtrsTriggers' - The JSON block of configuration information for each trigger.
--
-- * 'grtrsResponseStatus' - -- | The response status code.
getRepositoryTriggersResponse
    :: Int -- ^ 'grtrsResponseStatus'
    -> GetRepositoryTriggersResponse
getRepositoryTriggersResponse pResponseStatus_ =
  GetRepositoryTriggersResponse'
    { _grtrsConfigurationId = Nothing
    , _grtrsTriggers = Nothing
    , _grtrsResponseStatus = pResponseStatus_
    }


-- | The system-generated unique ID for the trigger.
grtrsConfigurationId :: Lens' GetRepositoryTriggersResponse (Maybe Text)
grtrsConfigurationId = lens _grtrsConfigurationId (\ s a -> s{_grtrsConfigurationId = a})

-- | The JSON block of configuration information for each trigger.
grtrsTriggers :: Lens' GetRepositoryTriggersResponse [RepositoryTrigger]
grtrsTriggers = lens _grtrsTriggers (\ s a -> s{_grtrsTriggers = a}) . _Default . _Coerce

-- | -- | The response status code.
grtrsResponseStatus :: Lens' GetRepositoryTriggersResponse Int
grtrsResponseStatus = lens _grtrsResponseStatus (\ s a -> s{_grtrsResponseStatus = a})

instance NFData GetRepositoryTriggersResponse where
