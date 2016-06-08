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
-- Module      : Network.AWS.CodeCommit.PutRepositoryTriggers
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces all triggers for a repository. This can be used to create or delete triggers.
module Network.AWS.CodeCommit.PutRepositoryTriggers
    (
    -- * Creating a Request
      putRepositoryTriggers
    , PutRepositoryTriggers
    -- * Request Lenses
    , prtTriggers
    , prtRepositoryName

    -- * Destructuring the Response
    , putRepositoryTriggersResponse
    , PutRepositoryTriggersResponse
    -- * Response Lenses
    , prtrsConfigurationId
    , prtrsResponseStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input ofa put repository triggers operation.
--
-- /See:/ 'putRepositoryTriggers' smart constructor.
data PutRepositoryTriggers = PutRepositoryTriggers'
    { _prtTriggers       :: !(Maybe [RepositoryTrigger])
    , _prtRepositoryName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRepositoryTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prtTriggers'
--
-- * 'prtRepositoryName'
putRepositoryTriggers
    :: PutRepositoryTriggers
putRepositoryTriggers =
    PutRepositoryTriggers'
    { _prtTriggers = Nothing
    , _prtRepositoryName = Nothing
    }

-- | The JSON block of configuration information for each trigger.
prtTriggers :: Lens' PutRepositoryTriggers [RepositoryTrigger]
prtTriggers = lens _prtTriggers (\ s a -> s{_prtTriggers = a}) . _Default . _Coerce;

-- | The name of the repository where you want to create or update the trigger.
prtRepositoryName :: Lens' PutRepositoryTriggers (Maybe Text)
prtRepositoryName = lens _prtRepositoryName (\ s a -> s{_prtRepositoryName = a});

instance AWSRequest PutRepositoryTriggers where
        type Rs PutRepositoryTriggers =
             PutRepositoryTriggersResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 PutRepositoryTriggersResponse' <$>
                   (x .?> "configurationId") <*> (pure (fromEnum s)))

instance Hashable PutRepositoryTriggers

instance NFData PutRepositoryTriggers

instance ToHeaders PutRepositoryTriggers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.PutRepositoryTriggers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRepositoryTriggers where
        toJSON PutRepositoryTriggers'{..}
          = object
              (catMaybes
                 [("triggers" .=) <$> _prtTriggers,
                  ("repositoryName" .=) <$> _prtRepositoryName])

instance ToPath PutRepositoryTriggers where
        toPath = const "/"

instance ToQuery PutRepositoryTriggers where
        toQuery = const mempty

-- | Represents the output of a put repository triggers operation.
--
-- /See:/ 'putRepositoryTriggersResponse' smart constructor.
data PutRepositoryTriggersResponse = PutRepositoryTriggersResponse'
    { _prtrsConfigurationId :: !(Maybe Text)
    , _prtrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRepositoryTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prtrsConfigurationId'
--
-- * 'prtrsResponseStatus'
putRepositoryTriggersResponse
    :: Int -- ^ 'prtrsResponseStatus'
    -> PutRepositoryTriggersResponse
putRepositoryTriggersResponse pResponseStatus_ =
    PutRepositoryTriggersResponse'
    { _prtrsConfigurationId = Nothing
    , _prtrsResponseStatus = pResponseStatus_
    }

-- | The system-generated unique ID for the create or update operation.
prtrsConfigurationId :: Lens' PutRepositoryTriggersResponse (Maybe Text)
prtrsConfigurationId = lens _prtrsConfigurationId (\ s a -> s{_prtrsConfigurationId = a});

-- | The response status code.
prtrsResponseStatus :: Lens' PutRepositoryTriggersResponse Int
prtrsResponseStatus = lens _prtrsResponseStatus (\ s a -> s{_prtrsResponseStatus = a});

instance NFData PutRepositoryTriggersResponse
