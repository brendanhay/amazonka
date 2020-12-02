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
-- Module      : Network.AWS.APIGateway.GetVPCLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a specified VPC link under the caller's account in a region.
--
--
module Network.AWS.APIGateway.GetVPCLink
    (
    -- * Creating a Request
      getVPCLink
    , GetVPCLink
    -- * Request Lenses
    , gvlVpcLinkId

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

-- | Gets a specified VPC link under the caller's account in a region.
--
--
--
-- /See:/ 'getVPCLink' smart constructor.
newtype GetVPCLink = GetVPCLink'
  { _gvlVpcLinkId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVPCLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvlVpcLinkId' - [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
getVPCLink
    :: Text -- ^ 'gvlVpcLinkId'
    -> GetVPCLink
getVPCLink pVpcLinkId_ = GetVPCLink' {_gvlVpcLinkId = pVpcLinkId_}


-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
gvlVpcLinkId :: Lens' GetVPCLink Text
gvlVpcLinkId = lens _gvlVpcLinkId (\ s a -> s{_gvlVpcLinkId = a})

instance AWSRequest GetVPCLink where
        type Rs GetVPCLink = VPCLink
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetVPCLink where

instance NFData GetVPCLink where

instance ToHeaders GetVPCLink where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetVPCLink where
        toPath GetVPCLink'{..}
          = mconcat ["/vpclinks/", toBS _gvlVpcLinkId]

instance ToQuery GetVPCLink where
        toQuery = const mempty
