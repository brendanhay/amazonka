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
-- Module      : Network.AWS.WorkMail.CreateResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail resource. The available types are equipment and room.
--
--
module Network.AWS.WorkMail.CreateResource
    (
    -- * Creating a Request
      createResource
    , CreateResource
    -- * Request Lenses
    , crOrganizationId
    , crName
    , crType

    -- * Destructuring the Response
    , createResourceResponse
    , CreateResourceResponse
    -- * Response Lenses
    , crrsResourceId
    , crrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'createResource' smart constructor.
data CreateResource = CreateResource'
  { _crOrganizationId :: !Text
  , _crName           :: !Text
  , _crType           :: !ResourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crOrganizationId' - The identifier associated with the organization for which the resource is created.
--
-- * 'crName' - The name of the created resource.
--
-- * 'crType' - The type of the created resource.
createResource
    :: Text -- ^ 'crOrganizationId'
    -> Text -- ^ 'crName'
    -> ResourceType -- ^ 'crType'
    -> CreateResource
createResource pOrganizationId_ pName_ pType_ =
  CreateResource'
    {_crOrganizationId = pOrganizationId_, _crName = pName_, _crType = pType_}


-- | The identifier associated with the organization for which the resource is created.
crOrganizationId :: Lens' CreateResource Text
crOrganizationId = lens _crOrganizationId (\ s a -> s{_crOrganizationId = a})

-- | The name of the created resource.
crName :: Lens' CreateResource Text
crName = lens _crName (\ s a -> s{_crName = a})

-- | The type of the created resource.
crType :: Lens' CreateResource ResourceType
crType = lens _crType (\ s a -> s{_crType = a})

instance AWSRequest CreateResource where
        type Rs CreateResource = CreateResourceResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 CreateResourceResponse' <$>
                   (x .?> "ResourceId") <*> (pure (fromEnum s)))

instance Hashable CreateResource where

instance NFData CreateResource where

instance ToHeaders CreateResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.CreateResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateResource where
        toJSON CreateResource'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _crOrganizationId),
                  Just ("Name" .= _crName), Just ("Type" .= _crType)])

instance ToPath CreateResource where
        toPath = const "/"

instance ToQuery CreateResource where
        toQuery = const mempty

-- | /See:/ 'createResourceResponse' smart constructor.
data CreateResourceResponse = CreateResourceResponse'
  { _crrsResourceId     :: !(Maybe Text)
  , _crrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsResourceId' - The identifier of the created resource.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createResourceResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateResourceResponse
createResourceResponse pResponseStatus_ =
  CreateResourceResponse'
    {_crrsResourceId = Nothing, _crrsResponseStatus = pResponseStatus_}


-- | The identifier of the created resource.
crrsResourceId :: Lens' CreateResourceResponse (Maybe Text)
crrsResourceId = lens _crrsResourceId (\ s a -> s{_crrsResourceId = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateResourceResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a})

instance NFData CreateResourceResponse where
