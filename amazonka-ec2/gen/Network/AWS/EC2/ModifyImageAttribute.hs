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
    , miarqAttribute
    , miarqUserIds
    , miarqUserGroups
    , miarqValue
    , miarqLaunchPermission
    , miarqOperationType
    , miarqProductCodes
    , miarqDryRun
    , miarqDescription
    , miarqImageId

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
-- * 'miarqAttribute'
--
-- * 'miarqUserIds'
--
-- * 'miarqUserGroups'
--
-- * 'miarqValue'
--
-- * 'miarqLaunchPermission'
--
-- * 'miarqOperationType'
--
-- * 'miarqProductCodes'
--
-- * 'miarqDryRun'
--
-- * 'miarqDescription'
--
-- * 'miarqImageId'
data ModifyImageAttribute = ModifyImageAttribute'
    { _miarqAttribute        :: !(Maybe Text)
    , _miarqUserIds          :: !(Maybe [Text])
    , _miarqUserGroups       :: !(Maybe [Text])
    , _miarqValue            :: !(Maybe Text)
    , _miarqLaunchPermission :: !(Maybe LaunchPermissionModifications)
    , _miarqOperationType    :: !(Maybe Text)
    , _miarqProductCodes     :: !(Maybe [Text])
    , _miarqDryRun           :: !(Maybe Bool)
    , _miarqDescription      :: !(Maybe AttributeValue)
    , _miarqImageId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyImageAttribute' smart constructor.
modifyImageAttribute :: Text -> ModifyImageAttribute
modifyImageAttribute pImageId =
    ModifyImageAttribute'
    { _miarqAttribute = Nothing
    , _miarqUserIds = Nothing
    , _miarqUserGroups = Nothing
    , _miarqValue = Nothing
    , _miarqLaunchPermission = Nothing
    , _miarqOperationType = Nothing
    , _miarqProductCodes = Nothing
    , _miarqDryRun = Nothing
    , _miarqDescription = Nothing
    , _miarqImageId = pImageId
    }

-- | The name of the attribute to modify.
miarqAttribute :: Lens' ModifyImageAttribute (Maybe Text)
miarqAttribute = lens _miarqAttribute (\ s a -> s{_miarqAttribute = a});

-- | One or more AWS account IDs. This is only valid when modifying the
-- @launchPermission@ attribute.
miarqUserIds :: Lens' ModifyImageAttribute [Text]
miarqUserIds = lens _miarqUserIds (\ s a -> s{_miarqUserIds = a}) . _Default;

-- | One or more user groups. This is only valid when modifying the
-- @launchPermission@ attribute.
miarqUserGroups :: Lens' ModifyImageAttribute [Text]
miarqUserGroups = lens _miarqUserGroups (\ s a -> s{_miarqUserGroups = a}) . _Default;

-- | The value of the attribute being modified. This is only valid when
-- modifying the @description@ attribute.
miarqValue :: Lens' ModifyImageAttribute (Maybe Text)
miarqValue = lens _miarqValue (\ s a -> s{_miarqValue = a});

-- | A launch permission modification.
miarqLaunchPermission :: Lens' ModifyImageAttribute (Maybe LaunchPermissionModifications)
miarqLaunchPermission = lens _miarqLaunchPermission (\ s a -> s{_miarqLaunchPermission = a});

-- | The operation type.
miarqOperationType :: Lens' ModifyImageAttribute (Maybe Text)
miarqOperationType = lens _miarqOperationType (\ s a -> s{_miarqOperationType = a});

-- | One or more product codes. After you add a product code to an AMI, it
-- can\'t be removed. This is only valid when modifying the @productCodes@
-- attribute.
miarqProductCodes :: Lens' ModifyImageAttribute [Text]
miarqProductCodes = lens _miarqProductCodes (\ s a -> s{_miarqProductCodes = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
miarqDryRun :: Lens' ModifyImageAttribute (Maybe Bool)
miarqDryRun = lens _miarqDryRun (\ s a -> s{_miarqDryRun = a});

-- | A description for the AMI.
miarqDescription :: Lens' ModifyImageAttribute (Maybe AttributeValue)
miarqDescription = lens _miarqDescription (\ s a -> s{_miarqDescription = a});

-- | The ID of the AMI.
miarqImageId :: Lens' ModifyImageAttribute Text
miarqImageId = lens _miarqImageId (\ s a -> s{_miarqImageId = a});

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
               "Attribute" =: _miarqAttribute,
               toQuery (toQueryList "UserId" <$> _miarqUserIds),
               toQuery
                 (toQueryList "UserGroup" <$> _miarqUserGroups),
               "Value" =: _miarqValue,
               "LaunchPermission" =: _miarqLaunchPermission,
               "OperationType" =: _miarqOperationType,
               toQuery
                 (toQueryList "ProductCode" <$> _miarqProductCodes),
               "DryRun" =: _miarqDryRun,
               "Description" =: _miarqDescription,
               "ImageId" =: _miarqImageId]

-- | /See:/ 'modifyImageAttributeResponse' smart constructor.
data ModifyImageAttributeResponse =
    ModifyImageAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyImageAttributeResponse' smart constructor.
modifyImageAttributeResponse :: ModifyImageAttributeResponse
modifyImageAttributeResponse = ModifyImageAttributeResponse'
