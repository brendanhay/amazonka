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
-- Module      : Network.AWS.APIGateway.DeleteDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Deployment' resource. Deleting a deployment will only succeed if there are no 'Stage' resources associated with it.
--
--
module Network.AWS.APIGateway.DeleteDeployment
    (
    -- * Creating a Request
      deleteDeployment
    , DeleteDeployment
    -- * Request Lenses
    , ddRestAPIId
    , ddDeploymentId

    -- * Destructuring the Response
    , deleteDeploymentResponse
    , DeleteDeploymentResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to delete a 'Deployment' resource.
--
--
--
-- /See:/ 'deleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { _ddRestAPIId    :: !Text
  , _ddDeploymentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'ddDeploymentId' - [Required] The identifier of the 'Deployment' resource to delete.
deleteDeployment
    :: Text -- ^ 'ddRestAPIId'
    -> Text -- ^ 'ddDeploymentId'
    -> DeleteDeployment
deleteDeployment pRestAPIId_ pDeploymentId_ =
  DeleteDeployment'
    {_ddRestAPIId = pRestAPIId_, _ddDeploymentId = pDeploymentId_}


-- | [Required] The string identifier of the associated 'RestApi' .
ddRestAPIId :: Lens' DeleteDeployment Text
ddRestAPIId = lens _ddRestAPIId (\ s a -> s{_ddRestAPIId = a})

-- | [Required] The identifier of the 'Deployment' resource to delete.
ddDeploymentId :: Lens' DeleteDeployment Text
ddDeploymentId = lens _ddDeploymentId (\ s a -> s{_ddDeploymentId = a})

instance AWSRequest DeleteDeployment where
        type Rs DeleteDeployment = DeleteDeploymentResponse
        request = delete apiGateway
        response = receiveNull DeleteDeploymentResponse'

instance Hashable DeleteDeployment where

instance NFData DeleteDeployment where

instance ToHeaders DeleteDeployment where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteDeployment where
        toPath DeleteDeployment'{..}
          = mconcat
              ["/restapis/", toBS _ddRestAPIId, "/deployments/",
               toBS _ddDeploymentId]

instance ToQuery DeleteDeployment where
        toQuery = const mempty

-- | /See:/ 'deleteDeploymentResponse' smart constructor.
data DeleteDeploymentResponse =
  DeleteDeploymentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeploymentResponse' with the minimum fields required to make a request.
--
deleteDeploymentResponse
    :: DeleteDeploymentResponse
deleteDeploymentResponse = DeleteDeploymentResponse'


instance NFData DeleteDeploymentResponse where
