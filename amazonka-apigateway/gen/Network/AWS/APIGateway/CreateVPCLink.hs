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
-- Module      : Network.AWS.APIGateway.CreateVPCLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.
--
--
module Network.AWS.APIGateway.CreateVPCLink
    (
    -- * Creating a Request
      createVPCLink
    , CreateVPCLink
    -- * Request Lenses
    , cvlDescription
    , cvlName
    , cvlTargetARNs

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

-- | Creates a VPC link, under the caller's account in a selected region, in an asynchronous operation that typically takes 2-4 minutes to complete and become operational. The caller must have permissions to create and update VPC Endpoint services.
--
--
--
-- /See:/ 'createVPCLink' smart constructor.
data CreateVPCLink = CreateVPCLink'
  { _cvlDescription :: !(Maybe Text)
  , _cvlName        :: !Text
  , _cvlTargetARNs  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvlDescription' - The description of the VPC link.
--
-- * 'cvlName' - [Required] The name used to label and identify the VPC link.
--
-- * 'cvlTargetARNs' - [Required] The ARNs of network load balancers of the VPC targeted by the VPC link. The network load balancers must be owned by the same AWS account of the API owner.
createVPCLink
    :: Text -- ^ 'cvlName'
    -> CreateVPCLink
createVPCLink pName_ =
  CreateVPCLink'
    {_cvlDescription = Nothing, _cvlName = pName_, _cvlTargetARNs = mempty}


-- | The description of the VPC link.
cvlDescription :: Lens' CreateVPCLink (Maybe Text)
cvlDescription = lens _cvlDescription (\ s a -> s{_cvlDescription = a})

-- | [Required] The name used to label and identify the VPC link.
cvlName :: Lens' CreateVPCLink Text
cvlName = lens _cvlName (\ s a -> s{_cvlName = a})

-- | [Required] The ARNs of network load balancers of the VPC targeted by the VPC link. The network load balancers must be owned by the same AWS account of the API owner.
cvlTargetARNs :: Lens' CreateVPCLink [Text]
cvlTargetARNs = lens _cvlTargetARNs (\ s a -> s{_cvlTargetARNs = a}) . _Coerce

instance AWSRequest CreateVPCLink where
        type Rs CreateVPCLink = VPCLink
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateVPCLink where

instance NFData CreateVPCLink where

instance ToHeaders CreateVPCLink where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateVPCLink where
        toJSON CreateVPCLink'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _cvlDescription,
                  Just ("name" .= _cvlName),
                  Just ("targetArns" .= _cvlTargetARNs)])

instance ToPath CreateVPCLink where
        toPath = const "/vpclinks"

instance ToQuery CreateVPCLink where
        toQuery = const mempty
