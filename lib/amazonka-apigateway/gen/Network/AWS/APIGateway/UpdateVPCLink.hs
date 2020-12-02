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
-- Module      : Network.AWS.APIGateway.UpdateVPCLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'VpcLink' of a specified identifier.
--
--
module Network.AWS.APIGateway.UpdateVPCLink
    (
    -- * Creating a Request
      updateVPCLink
    , UpdateVPCLink
    -- * Request Lenses
    , uvlPatchOperations
    , uvlVpcLinkId

    -- * Destructuring the Response
    , vpcLink
    , VPCLink
    -- * Response Lenses
    , vlStatus
    , vlTargetARNs
    , vlName
    , vlStatusMessage
    , vlId
    , vlDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates an existing 'VpcLink' of a specified identifier.
--
--
--
-- /See:/ 'updateVPCLink' smart constructor.
data UpdateVPCLink = UpdateVPCLink'
  { _uvlPatchOperations :: !(Maybe [PatchOperation])
  , _uvlVpcLinkId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVPCLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvlPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uvlVpcLinkId' - [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
updateVPCLink
    :: Text -- ^ 'uvlVpcLinkId'
    -> UpdateVPCLink
updateVPCLink pVpcLinkId_ =
  UpdateVPCLink' {_uvlPatchOperations = Nothing, _uvlVpcLinkId = pVpcLinkId_}


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uvlPatchOperations :: Lens' UpdateVPCLink [PatchOperation]
uvlPatchOperations = lens _uvlPatchOperations (\ s a -> s{_uvlPatchOperations = a}) . _Default . _Coerce

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
uvlVpcLinkId :: Lens' UpdateVPCLink Text
uvlVpcLinkId = lens _uvlVpcLinkId (\ s a -> s{_uvlVpcLinkId = a})

instance AWSRequest UpdateVPCLink where
        type Rs UpdateVPCLink = VPCLink
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateVPCLink where

instance NFData UpdateVPCLink where

instance ToHeaders UpdateVPCLink where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateVPCLink where
        toJSON UpdateVPCLink'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uvlPatchOperations])

instance ToPath UpdateVPCLink where
        toPath UpdateVPCLink'{..}
          = mconcat ["/vpclinks/", toBS _uvlVpcLinkId]

instance ToQuery UpdateVPCLink where
        toQuery = const mempty
