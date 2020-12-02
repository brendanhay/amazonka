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
-- Module      : Network.AWS.APIGateway.DeleteVPCLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'VpcLink' of a specified identifier.
--
--
module Network.AWS.APIGateway.DeleteVPCLink
    (
    -- * Creating a Request
      deleteVPCLink
    , DeleteVPCLink
    -- * Request Lenses
    , dvlVpcLinkId

    -- * Destructuring the Response
    , deleteVPCLinkResponse
    , DeleteVPCLinkResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes an existing 'VpcLink' of a specified identifier.
--
--
--
-- /See:/ 'deleteVPCLink' smart constructor.
newtype DeleteVPCLink = DeleteVPCLink'
  { _dvlVpcLinkId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvlVpcLinkId' - [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
deleteVPCLink
    :: Text -- ^ 'dvlVpcLinkId'
    -> DeleteVPCLink
deleteVPCLink pVpcLinkId_ = DeleteVPCLink' {_dvlVpcLinkId = pVpcLinkId_}


-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
dvlVpcLinkId :: Lens' DeleteVPCLink Text
dvlVpcLinkId = lens _dvlVpcLinkId (\ s a -> s{_dvlVpcLinkId = a})

instance AWSRequest DeleteVPCLink where
        type Rs DeleteVPCLink = DeleteVPCLinkResponse
        request = delete apiGateway
        response = receiveNull DeleteVPCLinkResponse'

instance Hashable DeleteVPCLink where

instance NFData DeleteVPCLink where

instance ToHeaders DeleteVPCLink where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteVPCLink where
        toPath DeleteVPCLink'{..}
          = mconcat ["/vpclinks/", toBS _dvlVpcLinkId]

instance ToQuery DeleteVPCLink where
        toQuery = const mempty

-- | /See:/ 'deleteVPCLinkResponse' smart constructor.
data DeleteVPCLinkResponse =
  DeleteVPCLinkResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCLinkResponse' with the minimum fields required to make a request.
--
deleteVPCLinkResponse
    :: DeleteVPCLinkResponse
deleteVPCLinkResponse = DeleteVPCLinkResponse'


instance NFData DeleteVPCLinkResponse where
