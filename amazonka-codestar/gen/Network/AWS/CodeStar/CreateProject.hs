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
-- Module      : Network.AWS.CodeStar.CreateProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserved for future use. To create a project, use the AWS CodeStar console.
--
--
module Network.AWS.CodeStar.CreateProject
    (
    -- * Creating a Request
      createProject
    , CreateProject
    -- * Request Lenses
    , cpClientRequestToken
    , cpDescription
    , cpName
    , cpId

    -- * Destructuring the Response
    , createProjectResponse
    , CreateProjectResponse
    -- * Response Lenses
    , cprsProjectTemplateId
    , cprsClientRequestToken
    , cprsResponseStatus
    , cprsId
    , cprsArn
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProject' smart constructor.
data CreateProject = CreateProject'
  { _cpClientRequestToken :: !(Maybe Text)
  , _cpDescription        :: !(Maybe (Sensitive Text))
  , _cpName               :: !(Sensitive Text)
  , _cpId                 :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpClientRequestToken' - Reserved for future use.
--
-- * 'cpDescription' - Reserved for future use.
--
-- * 'cpName' - Reserved for future use.
--
-- * 'cpId' - Reserved for future use.
createProject
    :: Text -- ^ 'cpName'
    -> Text -- ^ 'cpId'
    -> CreateProject
createProject pName_ pId_ =
  CreateProject'
    { _cpClientRequestToken = Nothing
    , _cpDescription = Nothing
    , _cpName = _Sensitive # pName_
    , _cpId = pId_
    }


-- | Reserved for future use.
cpClientRequestToken :: Lens' CreateProject (Maybe Text)
cpClientRequestToken = lens _cpClientRequestToken (\ s a -> s{_cpClientRequestToken = a})

-- | Reserved for future use.
cpDescription :: Lens' CreateProject (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a}) . mapping _Sensitive

-- | Reserved for future use.
cpName :: Lens' CreateProject Text
cpName = lens _cpName (\ s a -> s{_cpName = a}) . _Sensitive

-- | Reserved for future use.
cpId :: Lens' CreateProject Text
cpId = lens _cpId (\ s a -> s{_cpId = a})

instance AWSRequest CreateProject where
        type Rs CreateProject = CreateProjectResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 CreateProjectResponse' <$>
                   (x .?> "projectTemplateId") <*>
                     (x .?> "clientRequestToken")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "id")
                     <*> (x .:> "arn"))

instance Hashable CreateProject where

instance NFData CreateProject where

instance ToHeaders CreateProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.CreateProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProject where
        toJSON CreateProject'{..}
          = object
              (catMaybes
                 [("clientRequestToken" .=) <$> _cpClientRequestToken,
                  ("description" .=) <$> _cpDescription,
                  Just ("name" .= _cpName), Just ("id" .= _cpId)])

instance ToPath CreateProject where
        toPath = const "/"

instance ToQuery CreateProject where
        toQuery = const mempty

-- | /See:/ 'createProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { _cprsProjectTemplateId  :: !(Maybe Text)
  , _cprsClientRequestToken :: !(Maybe Text)
  , _cprsResponseStatus     :: !Int
  , _cprsId                 :: !Text
  , _cprsArn                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProjectTemplateId' - Reserved for future use.
--
-- * 'cprsClientRequestToken' - Reserved for future use.
--
-- * 'cprsResponseStatus' - -- | The response status code.
--
-- * 'cprsId' - Reserved for future use.
--
-- * 'cprsArn' - Reserved for future use.
createProjectResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> Text -- ^ 'cprsId'
    -> Text -- ^ 'cprsArn'
    -> CreateProjectResponse
createProjectResponse pResponseStatus_ pId_ pArn_ =
  CreateProjectResponse'
    { _cprsProjectTemplateId = Nothing
    , _cprsClientRequestToken = Nothing
    , _cprsResponseStatus = pResponseStatus_
    , _cprsId = pId_
    , _cprsArn = pArn_
    }


-- | Reserved for future use.
cprsProjectTemplateId :: Lens' CreateProjectResponse (Maybe Text)
cprsProjectTemplateId = lens _cprsProjectTemplateId (\ s a -> s{_cprsProjectTemplateId = a})

-- | Reserved for future use.
cprsClientRequestToken :: Lens' CreateProjectResponse (Maybe Text)
cprsClientRequestToken = lens _cprsClientRequestToken (\ s a -> s{_cprsClientRequestToken = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProjectResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

-- | Reserved for future use.
cprsId :: Lens' CreateProjectResponse Text
cprsId = lens _cprsId (\ s a -> s{_cprsId = a})

-- | Reserved for future use.
cprsArn :: Lens' CreateProjectResponse Text
cprsArn = lens _cprsArn (\ s a -> s{_cprsArn = a})

instance NFData CreateProjectResponse where
