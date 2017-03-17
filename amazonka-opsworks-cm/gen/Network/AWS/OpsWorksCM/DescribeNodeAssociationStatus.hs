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
-- Module      : Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of an existing association or disassociation request.
--
--
-- A @ResourceNotFoundException@ is thrown when no recent association or disassociation request with the specified token is found, or when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
    (
    -- * Creating a Request
      describeNodeAssociationStatus
    , DescribeNodeAssociationStatus
    -- * Request Lenses
    , dnasNodeAssociationStatusToken
    , dnasServerName

    -- * Destructuring the Response
    , describeNodeAssociationStatusResponse
    , DescribeNodeAssociationStatusResponse
    -- * Response Lenses
    , dnasrsResponseStatus
    , dnasrsNodeAssociationStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.OpsWorksCM.Types
import           Network.AWS.OpsWorksCM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeNodeAssociationStatus' smart constructor.
data DescribeNodeAssociationStatus = DescribeNodeAssociationStatus'
    { _dnasNodeAssociationStatusToken :: !Text
    , _dnasServerName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNodeAssociationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnasNodeAssociationStatusToken' - Undocumented member.
--
-- * 'dnasServerName' - The name of the server from which to disassociate the node.
describeNodeAssociationStatus
    :: Text -- ^ 'dnasNodeAssociationStatusToken'
    -> Text -- ^ 'dnasServerName'
    -> DescribeNodeAssociationStatus
describeNodeAssociationStatus pNodeAssociationStatusToken_ pServerName_ =
    DescribeNodeAssociationStatus'
    { _dnasNodeAssociationStatusToken = pNodeAssociationStatusToken_
    , _dnasServerName = pServerName_
    }

-- | Undocumented member.
dnasNodeAssociationStatusToken :: Lens' DescribeNodeAssociationStatus Text
dnasNodeAssociationStatusToken = lens _dnasNodeAssociationStatusToken (\ s a -> s{_dnasNodeAssociationStatusToken = a});

-- | The name of the server from which to disassociate the node.
dnasServerName :: Lens' DescribeNodeAssociationStatus Text
dnasServerName = lens _dnasServerName (\ s a -> s{_dnasServerName = a});

instance AWSRequest DescribeNodeAssociationStatus
         where
        type Rs DescribeNodeAssociationStatus =
             DescribeNodeAssociationStatusResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeNodeAssociationStatusResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .:> "NodeAssociationStatus"))

instance Hashable DescribeNodeAssociationStatus

instance NFData DescribeNodeAssociationStatus

instance ToHeaders DescribeNodeAssociationStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.DescribeNodeAssociationStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeNodeAssociationStatus where
        toJSON DescribeNodeAssociationStatus'{..}
          = object
              (catMaybes
                 [Just
                    ("NodeAssociationStatusToken" .=
                       _dnasNodeAssociationStatusToken),
                  Just ("ServerName" .= _dnasServerName)])

instance ToPath DescribeNodeAssociationStatus where
        toPath = const "/"

instance ToQuery DescribeNodeAssociationStatus where
        toQuery = const mempty

-- | /See:/ 'describeNodeAssociationStatusResponse' smart constructor.
data DescribeNodeAssociationStatusResponse = DescribeNodeAssociationStatusResponse'
    { _dnasrsResponseStatus        :: !Int
    , _dnasrsNodeAssociationStatus :: !NodeAssociationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNodeAssociationStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnasrsResponseStatus' - -- | The response status code.
--
-- * 'dnasrsNodeAssociationStatus' - The status of the association or disassociation request.  __Possible values:__      * @SUCCESS@ : The association or disassociation succeeded.      * @FAILED@ : The association or disassociation failed.      * @IN_PROGRESS@ : The association or disassociation is still in progress.
describeNodeAssociationStatusResponse
    :: Int -- ^ 'dnasrsResponseStatus'
    -> NodeAssociationStatus -- ^ 'dnasrsNodeAssociationStatus'
    -> DescribeNodeAssociationStatusResponse
describeNodeAssociationStatusResponse pResponseStatus_ pNodeAssociationStatus_ =
    DescribeNodeAssociationStatusResponse'
    { _dnasrsResponseStatus = pResponseStatus_
    , _dnasrsNodeAssociationStatus = pNodeAssociationStatus_
    }

-- | -- | The response status code.
dnasrsResponseStatus :: Lens' DescribeNodeAssociationStatusResponse Int
dnasrsResponseStatus = lens _dnasrsResponseStatus (\ s a -> s{_dnasrsResponseStatus = a});

-- | The status of the association or disassociation request.  __Possible values:__      * @SUCCESS@ : The association or disassociation succeeded.      * @FAILED@ : The association or disassociation failed.      * @IN_PROGRESS@ : The association or disassociation is still in progress.
dnasrsNodeAssociationStatus :: Lens' DescribeNodeAssociationStatusResponse NodeAssociationStatus
dnasrsNodeAssociationStatus = lens _dnasrsNodeAssociationStatus (\ s a -> s{_dnasrsNodeAssociationStatus = a});

instance NFData DescribeNodeAssociationStatusResponse
