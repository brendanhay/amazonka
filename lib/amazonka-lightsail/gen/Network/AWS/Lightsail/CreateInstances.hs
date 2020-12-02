{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more Amazon Lightsail instances.
--
--
-- The @create instances@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateInstances
  ( -- * Creating a Request
    createInstances,
    CreateInstances,

    -- * Request Lenses
    ciCustomImageName,
    ciAddOns,
    ciUserData,
    ciKeyPairName,
    ciTags,
    ciInstanceNames,
    ciAvailabilityZone,
    ciBlueprintId,
    ciBundleId,

    -- * Destructuring the Response
    createInstancesResponse,
    CreateInstancesResponse,

    -- * Response Lenses
    cirsOperations,
    cirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstances' smart constructor.
data CreateInstances = CreateInstances'
  { _ciCustomImageName ::
      !(Maybe Text),
    _ciAddOns :: !(Maybe [AddOnRequest]),
    _ciUserData :: !(Maybe Text),
    _ciKeyPairName :: !(Maybe Text),
    _ciTags :: !(Maybe [Tag]),
    _ciInstanceNames :: ![Text],
    _ciAvailabilityZone :: !Text,
    _ciBlueprintId :: !Text,
    _ciBundleId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciCustomImageName' - (Deprecated) The name for your custom image.
--
-- * 'ciAddOns' - An array of objects representing the add-ons to enable for the new instance.
--
-- * 'ciUserData' - A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- * 'ciKeyPairName' - The name of your key pair.
--
-- * 'ciTags' - The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
--
-- * 'ciInstanceNames' - The names to use for your new Lightsail instances. Separate multiple values using quotation marks and commas, for example: @["MyFirstInstance","MySecondInstance"]@
--
-- * 'ciAvailabilityZone' - The Availability Zone in which to create your instance. Use the following format: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
--
-- * 'ciBlueprintId' - The ID for a virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ). Use the @get blueprints@ operation to return a list of available images (or /blueprints/ ).
--
-- * 'ciBundleId' - The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
createInstances ::
  -- | 'ciAvailabilityZone'
  Text ->
  -- | 'ciBlueprintId'
  Text ->
  -- | 'ciBundleId'
  Text ->
  CreateInstances
createInstances pAvailabilityZone_ pBlueprintId_ pBundleId_ =
  CreateInstances'
    { _ciCustomImageName = Nothing,
      _ciAddOns = Nothing,
      _ciUserData = Nothing,
      _ciKeyPairName = Nothing,
      _ciTags = Nothing,
      _ciInstanceNames = mempty,
      _ciAvailabilityZone = pAvailabilityZone_,
      _ciBlueprintId = pBlueprintId_,
      _ciBundleId = pBundleId_
    }

-- | (Deprecated) The name for your custom image.
ciCustomImageName :: Lens' CreateInstances (Maybe Text)
ciCustomImageName = lens _ciCustomImageName (\s a -> s {_ciCustomImageName = a})

-- | An array of objects representing the add-ons to enable for the new instance.
ciAddOns :: Lens' CreateInstances [AddOnRequest]
ciAddOns = lens _ciAddOns (\s a -> s {_ciAddOns = a}) . _Default . _Coerce

-- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
ciUserData :: Lens' CreateInstances (Maybe Text)
ciUserData = lens _ciUserData (\s a -> s {_ciUserData = a})

-- | The name of your key pair.
ciKeyPairName :: Lens' CreateInstances (Maybe Text)
ciKeyPairName = lens _ciKeyPairName (\s a -> s {_ciKeyPairName = a})

-- | The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
ciTags :: Lens' CreateInstances [Tag]
ciTags = lens _ciTags (\s a -> s {_ciTags = a}) . _Default . _Coerce

-- | The names to use for your new Lightsail instances. Separate multiple values using quotation marks and commas, for example: @["MyFirstInstance","MySecondInstance"]@
ciInstanceNames :: Lens' CreateInstances [Text]
ciInstanceNames = lens _ciInstanceNames (\s a -> s {_ciInstanceNames = a}) . _Coerce

-- | The Availability Zone in which to create your instance. Use the following format: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
ciAvailabilityZone :: Lens' CreateInstances Text
ciAvailabilityZone = lens _ciAvailabilityZone (\s a -> s {_ciAvailabilityZone = a})

-- | The ID for a virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ). Use the @get blueprints@ operation to return a list of available images (or /blueprints/ ).
ciBlueprintId :: Lens' CreateInstances Text
ciBlueprintId = lens _ciBlueprintId (\s a -> s {_ciBlueprintId = a})

-- | The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
ciBundleId :: Lens' CreateInstances Text
ciBundleId = lens _ciBundleId (\s a -> s {_ciBundleId = a})

instance AWSRequest CreateInstances where
  type Rs CreateInstances = CreateInstancesResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateInstancesResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateInstances

instance NFData CreateInstances

instance ToHeaders CreateInstances where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateInstances" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateInstances where
  toJSON CreateInstances' {..} =
    object
      ( catMaybes
          [ ("customImageName" .=) <$> _ciCustomImageName,
            ("addOns" .=) <$> _ciAddOns,
            ("userData" .=) <$> _ciUserData,
            ("keyPairName" .=) <$> _ciKeyPairName,
            ("tags" .=) <$> _ciTags,
            Just ("instanceNames" .= _ciInstanceNames),
            Just ("availabilityZone" .= _ciAvailabilityZone),
            Just ("blueprintId" .= _ciBlueprintId),
            Just ("bundleId" .= _ciBundleId)
          ]
      )

instance ToPath CreateInstances where
  toPath = const "/"

instance ToQuery CreateInstances where
  toQuery = const mempty

-- | /See:/ 'createInstancesResponse' smart constructor.
data CreateInstancesResponse = CreateInstancesResponse'
  { _cirsOperations ::
      !(Maybe [Operation]),
    _cirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'cirsResponseStatus' - -- | The response status code.
createInstancesResponse ::
  -- | 'cirsResponseStatus'
  Int ->
  CreateInstancesResponse
createInstancesResponse pResponseStatus_ =
  CreateInstancesResponse'
    { _cirsOperations = Nothing,
      _cirsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
cirsOperations :: Lens' CreateInstancesResponse [Operation]
cirsOperations = lens _cirsOperations (\s a -> s {_cirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
cirsResponseStatus :: Lens' CreateInstancesResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\s a -> s {_cirsResponseStatus = a})

instance NFData CreateInstancesResponse
