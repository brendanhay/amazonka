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
-- Module      : Network.AWS.APIGateway.UpdateDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Deployment' resource.
--
--
module Network.AWS.APIGateway.UpdateDeployment
    (
    -- * Creating a Request
      updateDeployment
    , UpdateDeployment
    -- * Request Lenses
    , udPatchOperations
    , udRestAPIId
    , udDeploymentId

    -- * Destructuring the Response
    , deployment
    , Deployment
    -- * Response Lenses
    , dApiSummary
    , dCreatedDate
    , dId
    , dDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to change information about a 'Deployment' resource.
--
--
--
-- /See:/ 'updateDeployment' smart constructor.
data UpdateDeployment = UpdateDeployment'
  { _udPatchOperations :: !(Maybe [PatchOperation])
  , _udRestAPIId       :: !Text
  , _udDeploymentId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'udRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'udDeploymentId' - The replacement identifier for the 'Deployment' resource to change information about.
updateDeployment
    :: Text -- ^ 'udRestAPIId'
    -> Text -- ^ 'udDeploymentId'
    -> UpdateDeployment
updateDeployment pRestAPIId_ pDeploymentId_ =
  UpdateDeployment'
    { _udPatchOperations = Nothing
    , _udRestAPIId = pRestAPIId_
    , _udDeploymentId = pDeploymentId_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
udPatchOperations :: Lens' UpdateDeployment [PatchOperation]
udPatchOperations = lens _udPatchOperations (\ s a -> s{_udPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
udRestAPIId :: Lens' UpdateDeployment Text
udRestAPIId = lens _udRestAPIId (\ s a -> s{_udRestAPIId = a})

-- | The replacement identifier for the 'Deployment' resource to change information about.
udDeploymentId :: Lens' UpdateDeployment Text
udDeploymentId = lens _udDeploymentId (\ s a -> s{_udDeploymentId = a})

instance AWSRequest UpdateDeployment where
        type Rs UpdateDeployment = Deployment
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateDeployment where

instance NFData UpdateDeployment where

instance ToHeaders UpdateDeployment where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateDeployment where
        toJSON UpdateDeployment'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _udPatchOperations])

instance ToPath UpdateDeployment where
        toPath UpdateDeployment'{..}
          = mconcat
              ["/restapis/", toBS _udRestAPIId, "/deployments/",
               toBS _udDeploymentId]

instance ToQuery UpdateDeployment where
        toQuery = const mempty
