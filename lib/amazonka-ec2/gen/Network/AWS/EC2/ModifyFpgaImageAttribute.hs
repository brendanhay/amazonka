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
-- Module      : Network.AWS.EC2.ModifyFpgaImageAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified Amazon FPGA Image (AFI).
--
--
module Network.AWS.EC2.ModifyFpgaImageAttribute
    (
    -- * Creating a Request
      modifyFpgaImageAttribute
    , ModifyFpgaImageAttribute
    -- * Request Lenses
    , mfiaAttribute
    , mfiaUserIds
    , mfiaUserGroups
    , mfiaLoadPermission
    , mfiaName
    , mfiaOperationType
    , mfiaProductCodes
    , mfiaDescription
    , mfiaDryRun
    , mfiaFpgaImageId

    -- * Destructuring the Response
    , modifyFpgaImageAttributeResponse
    , ModifyFpgaImageAttributeResponse
    -- * Response Lenses
    , mfiarsFpgaImageAttribute
    , mfiarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyFpgaImageAttribute' smart constructor.
data ModifyFpgaImageAttribute = ModifyFpgaImageAttribute'
  { _mfiaAttribute      :: !(Maybe FpgaImageAttributeName)
  , _mfiaUserIds        :: !(Maybe [Text])
  , _mfiaUserGroups     :: !(Maybe [Text])
  , _mfiaLoadPermission :: !(Maybe LoadPermissionModifications)
  , _mfiaName           :: !(Maybe Text)
  , _mfiaOperationType  :: !(Maybe OperationType)
  , _mfiaProductCodes   :: !(Maybe [Text])
  , _mfiaDescription    :: !(Maybe Text)
  , _mfiaDryRun         :: !(Maybe Bool)
  , _mfiaFpgaImageId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyFpgaImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfiaAttribute' - The name of the attribute.
--
-- * 'mfiaUserIds' - One or more AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- * 'mfiaUserGroups' - One or more user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
--
-- * 'mfiaLoadPermission' - The load permission for the AFI.
--
-- * 'mfiaName' - A name for the AFI.
--
-- * 'mfiaOperationType' - The operation type.
--
-- * 'mfiaProductCodes' - One or more product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
--
-- * 'mfiaDescription' - A description for the AFI.
--
-- * 'mfiaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mfiaFpgaImageId' - The ID of the AFI.
modifyFpgaImageAttribute
    :: Text -- ^ 'mfiaFpgaImageId'
    -> ModifyFpgaImageAttribute
modifyFpgaImageAttribute pFpgaImageId_ =
  ModifyFpgaImageAttribute'
    { _mfiaAttribute = Nothing
    , _mfiaUserIds = Nothing
    , _mfiaUserGroups = Nothing
    , _mfiaLoadPermission = Nothing
    , _mfiaName = Nothing
    , _mfiaOperationType = Nothing
    , _mfiaProductCodes = Nothing
    , _mfiaDescription = Nothing
    , _mfiaDryRun = Nothing
    , _mfiaFpgaImageId = pFpgaImageId_
    }


-- | The name of the attribute.
mfiaAttribute :: Lens' ModifyFpgaImageAttribute (Maybe FpgaImageAttributeName)
mfiaAttribute = lens _mfiaAttribute (\ s a -> s{_mfiaAttribute = a})

-- | One or more AWS account IDs. This parameter is valid only when modifying the @loadPermission@ attribute.
mfiaUserIds :: Lens' ModifyFpgaImageAttribute [Text]
mfiaUserIds = lens _mfiaUserIds (\ s a -> s{_mfiaUserIds = a}) . _Default . _Coerce

-- | One or more user groups. This parameter is valid only when modifying the @loadPermission@ attribute.
mfiaUserGroups :: Lens' ModifyFpgaImageAttribute [Text]
mfiaUserGroups = lens _mfiaUserGroups (\ s a -> s{_mfiaUserGroups = a}) . _Default . _Coerce

-- | The load permission for the AFI.
mfiaLoadPermission :: Lens' ModifyFpgaImageAttribute (Maybe LoadPermissionModifications)
mfiaLoadPermission = lens _mfiaLoadPermission (\ s a -> s{_mfiaLoadPermission = a})

-- | A name for the AFI.
mfiaName :: Lens' ModifyFpgaImageAttribute (Maybe Text)
mfiaName = lens _mfiaName (\ s a -> s{_mfiaName = a})

-- | The operation type.
mfiaOperationType :: Lens' ModifyFpgaImageAttribute (Maybe OperationType)
mfiaOperationType = lens _mfiaOperationType (\ s a -> s{_mfiaOperationType = a})

-- | One or more product codes. After you add a product code to an AFI, it can't be removed. This parameter is valid only when modifying the @productCodes@ attribute.
mfiaProductCodes :: Lens' ModifyFpgaImageAttribute [Text]
mfiaProductCodes = lens _mfiaProductCodes (\ s a -> s{_mfiaProductCodes = a}) . _Default . _Coerce

-- | A description for the AFI.
mfiaDescription :: Lens' ModifyFpgaImageAttribute (Maybe Text)
mfiaDescription = lens _mfiaDescription (\ s a -> s{_mfiaDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mfiaDryRun :: Lens' ModifyFpgaImageAttribute (Maybe Bool)
mfiaDryRun = lens _mfiaDryRun (\ s a -> s{_mfiaDryRun = a})

-- | The ID of the AFI.
mfiaFpgaImageId :: Lens' ModifyFpgaImageAttribute Text
mfiaFpgaImageId = lens _mfiaFpgaImageId (\ s a -> s{_mfiaFpgaImageId = a})

instance AWSRequest ModifyFpgaImageAttribute where
        type Rs ModifyFpgaImageAttribute =
             ModifyFpgaImageAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyFpgaImageAttributeResponse' <$>
                   (x .@? "fpgaImageAttribute") <*> (pure (fromEnum s)))

instance Hashable ModifyFpgaImageAttribute where

instance NFData ModifyFpgaImageAttribute where

instance ToHeaders ModifyFpgaImageAttribute where
        toHeaders = const mempty

instance ToPath ModifyFpgaImageAttribute where
        toPath = const "/"

instance ToQuery ModifyFpgaImageAttribute where
        toQuery ModifyFpgaImageAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifyFpgaImageAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Attribute" =: _mfiaAttribute,
               toQuery (toQueryList "UserId" <$> _mfiaUserIds),
               toQuery
                 (toQueryList "UserGroup" <$> _mfiaUserGroups),
               "LoadPermission" =: _mfiaLoadPermission,
               "Name" =: _mfiaName,
               "OperationType" =: _mfiaOperationType,
               toQuery
                 (toQueryList "ProductCode" <$> _mfiaProductCodes),
               "Description" =: _mfiaDescription,
               "DryRun" =: _mfiaDryRun,
               "FpgaImageId" =: _mfiaFpgaImageId]

-- | /See:/ 'modifyFpgaImageAttributeResponse' smart constructor.
data ModifyFpgaImageAttributeResponse = ModifyFpgaImageAttributeResponse'
  { _mfiarsFpgaImageAttribute :: !(Maybe FpgaImageAttribute)
  , _mfiarsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyFpgaImageAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfiarsFpgaImageAttribute' - Information about the attribute.
--
-- * 'mfiarsResponseStatus' - -- | The response status code.
modifyFpgaImageAttributeResponse
    :: Int -- ^ 'mfiarsResponseStatus'
    -> ModifyFpgaImageAttributeResponse
modifyFpgaImageAttributeResponse pResponseStatus_ =
  ModifyFpgaImageAttributeResponse'
    { _mfiarsFpgaImageAttribute = Nothing
    , _mfiarsResponseStatus = pResponseStatus_
    }


-- | Information about the attribute.
mfiarsFpgaImageAttribute :: Lens' ModifyFpgaImageAttributeResponse (Maybe FpgaImageAttribute)
mfiarsFpgaImageAttribute = lens _mfiarsFpgaImageAttribute (\ s a -> s{_mfiarsFpgaImageAttribute = a})

-- | -- | The response status code.
mfiarsResponseStatus :: Lens' ModifyFpgaImageAttributeResponse Int
mfiarsResponseStatus = lens _mfiarsResponseStatus (\ s a -> s{_mfiarsResponseStatus = a})

instance NFData ModifyFpgaImageAttributeResponse
         where
