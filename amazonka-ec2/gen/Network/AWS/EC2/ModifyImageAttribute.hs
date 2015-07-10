{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyImageAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified AMI. You can specify
-- only one attribute at a time.
--
-- AWS Marketplace product codes cannot be modified. Images with an AWS
-- Marketplace product code cannot be made public.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyImageAttribute.html>
module Network.AWS.EC2.ModifyImageAttribute
    (
    -- * Request
      ModifyImageAttribute
    -- ** Request constructor
    , modifyImageAttribute
    -- ** Request lenses
    , miaAttribute
    , miaUserIds
    , miaUserGroups
    , miaValue
    , miaLaunchPermission
    , miaOperationType
    , miaProductCodes
    , miaDryRun
    , miaDescription
    , miaImageId

    -- * Response
    , ModifyImageAttributeResponse
    -- ** Response constructor
    , modifyImageAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyImageAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'miaAttribute'
--
-- * 'miaUserIds'
--
-- * 'miaUserGroups'
--
-- * 'miaValue'
--
-- * 'miaLaunchPermission'
--
-- * 'miaOperationType'
--
-- * 'miaProductCodes'
--
-- * 'miaDryRun'
--
-- * 'miaDescription'
--
-- * 'miaImageId'
data ModifyImageAttribute = ModifyImageAttribute'
    { _miaAttribute        :: !(Maybe Text)
    , _miaUserIds          :: !(Maybe [Text])
    , _miaUserGroups       :: !(Maybe [Text])
    , _miaValue            :: !(Maybe Text)
    , _miaLaunchPermission :: !(Maybe LaunchPermissionModifications)
    , _miaOperationType    :: !(Maybe Text)
    , _miaProductCodes     :: !(Maybe [Text])
    , _miaDryRun           :: !(Maybe Bool)
    , _miaDescription      :: !(Maybe AttributeValue)
    , _miaImageId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyImageAttribute' smart constructor.
modifyImageAttribute :: Text -> ModifyImageAttribute
modifyImageAttribute pImageId =
    ModifyImageAttribute'
    { _miaAttribute = Nothing
    , _miaUserIds = Nothing
    , _miaUserGroups = Nothing
    , _miaValue = Nothing
    , _miaLaunchPermission = Nothing
    , _miaOperationType = Nothing
    , _miaProductCodes = Nothing
    , _miaDryRun = Nothing
    , _miaDescription = Nothing
    , _miaImageId = pImageId
    }

-- | The name of the attribute to modify.
miaAttribute :: Lens' ModifyImageAttribute (Maybe Text)
miaAttribute = lens _miaAttribute (\ s a -> s{_miaAttribute = a});

-- | One or more AWS account IDs. This is only valid when modifying the
-- @launchPermission@ attribute.
miaUserIds :: Lens' ModifyImageAttribute [Text]
miaUserIds = lens _miaUserIds (\ s a -> s{_miaUserIds = a}) . _Default;

-- | One or more user groups. This is only valid when modifying the
-- @launchPermission@ attribute.
miaUserGroups :: Lens' ModifyImageAttribute [Text]
miaUserGroups = lens _miaUserGroups (\ s a -> s{_miaUserGroups = a}) . _Default;

-- | The value of the attribute being modified. This is only valid when
-- modifying the @description@ attribute.
miaValue :: Lens' ModifyImageAttribute (Maybe Text)
miaValue = lens _miaValue (\ s a -> s{_miaValue = a});

-- | A launch permission modification.
miaLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)
miaLaunchPermission = lens _miaLaunchPermission (\ s a -> s{_miaLaunchPermission = a});

-- | The operation type.
miaOperationType :: Lens' ModifyImageAttribute (Maybe Text)
miaOperationType = lens _miaOperationType (\ s a -> s{_miaOperationType = a});

-- | One or more product codes. After you add a product code to an AMI, it
-- can\'t be removed. This is only valid when modifying the @productCodes@
-- attribute.
miaProductCodes :: Lens' ModifyImageAttribute [Text]
miaProductCodes = lens _miaProductCodes (\ s a -> s{_miaProductCodes = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
miaDryRun :: Lens' ModifyImageAttribute (Maybe Bool)
miaDryRun = lens _miaDryRun (\ s a -> s{_miaDryRun = a});

-- | A description for the AMI.
miaDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)
miaDescription = lens _miaDescription (\ s a -> s{_miaDescription = a});

-- | The ID of the AMI.
miaImageId :: Lens' ModifyImageAttribute Text
miaImageId = lens _miaImageId (\ s a -> s{_miaImageId = a});

instance AWSRequest ModifyImageAttribute where
        type Sv ModifyImageAttribute = EC2
        type Rs ModifyImageAttribute =
             ModifyImageAttributeResponse
        request = post
        response = receiveNull ModifyImageAttributeResponse'

instance ToHeaders ModifyImageAttribute where
        toHeaders = const mempty

instance ToPath ModifyImageAttribute where
        toPath = const "/"

instance ToQuery ModifyImageAttribute where
        toQuery ModifyImageAttribute'{..}
          = mconcat
              ["Action" =: ("ModifyImageAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Attribute" =: _miaAttribute,
               toQuery (toQueryList "UserId" <$> _miaUserIds),
               toQuery (toQueryList "UserGroup" <$> _miaUserGroups),
               "Value" =: _miaValue,
               "LaunchPermission" =: _miaLaunchPermission,
               "OperationType" =: _miaOperationType,
               toQuery
                 (toQueryList "ProductCode" <$> _miaProductCodes),
               "DryRun" =: _miaDryRun,
               "Description" =: _miaDescription,
               "ImageId" =: _miaImageId]

-- | /See:/ 'modifyImageAttributeResponse' smart constructor.
data ModifyImageAttributeResponse =
    ModifyImageAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyImageAttributeResponse' smart constructor.
modifyImageAttributeResponse :: ModifyImageAttributeResponse
modifyImageAttributeResponse = ModifyImageAttributeResponse'
