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
-- Module      : Network.AWS.IoT.DescribeThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing.
module Network.AWS.IoT.DescribeThing
  ( -- * Creating a Request
    describeThing,
    DescribeThing,

    -- * Request Lenses
    desThingName,

    -- * Destructuring the Response
    describeThingResponse,
    DescribeThingResponse,

    -- * Response Lenses
    dtrsDefaultClientId,
    dtrsThingTypeName,
    dtrsThingARN,
    dtrsAttributes,
    dtrsVersion,
    dtrsThingName,
    dtrsBillingGroupName,
    dtrsThingId,
    dtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DescribeThing operation.
--
--
--
-- /See:/ 'describeThing' smart constructor.
newtype DescribeThing = DescribeThing' {_desThingName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desThingName' - The name of the thing.
describeThing ::
  -- | 'desThingName'
  Text ->
  DescribeThing
describeThing pThingName_ =
  DescribeThing' {_desThingName = pThingName_}

-- | The name of the thing.
desThingName :: Lens' DescribeThing Text
desThingName = lens _desThingName (\s a -> s {_desThingName = a})

instance AWSRequest DescribeThing where
  type Rs DescribeThing = DescribeThingResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeThingResponse'
            <$> (x .?> "defaultClientId")
            <*> (x .?> "thingTypeName")
            <*> (x .?> "thingArn")
            <*> (x .?> "attributes" .!@ mempty)
            <*> (x .?> "version")
            <*> (x .?> "thingName")
            <*> (x .?> "billingGroupName")
            <*> (x .?> "thingId")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeThing

instance NFData DescribeThing

instance ToHeaders DescribeThing where
  toHeaders = const mempty

instance ToPath DescribeThing where
  toPath DescribeThing' {..} =
    mconcat ["/things/", toBS _desThingName]

instance ToQuery DescribeThing where
  toQuery = const mempty

-- | The output from the DescribeThing operation.
--
--
--
-- /See:/ 'describeThingResponse' smart constructor.
data DescribeThingResponse = DescribeThingResponse'
  { _dtrsDefaultClientId ::
      !(Maybe Text),
    _dtrsThingTypeName :: !(Maybe Text),
    _dtrsThingARN :: !(Maybe Text),
    _dtrsAttributes :: !(Maybe (Map Text (Text))),
    _dtrsVersion :: !(Maybe Integer),
    _dtrsThingName :: !(Maybe Text),
    _dtrsBillingGroupName :: !(Maybe Text),
    _dtrsThingId :: !(Maybe Text),
    _dtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsDefaultClientId' - The default MQTT client ID. For a typical device, the thing name is also used as the default MQTT client ID. Although we don’t require a mapping between a thing's registry name and its use of MQTT client IDs, certificates, or shadow state, we recommend that you choose a thing name and use it as the MQTT client ID for the registry and the Device Shadow service. This lets you better organize your AWS IoT fleet without removing the flexibility of the underlying device certificate model or shadows.
--
-- * 'dtrsThingTypeName' - The thing type name.
--
-- * 'dtrsThingARN' - The ARN of the thing to describe.
--
-- * 'dtrsAttributes' - The thing attributes.
--
-- * 'dtrsVersion' - The current version of the thing record in the registry.
--
-- * 'dtrsThingName' - The name of the thing.
--
-- * 'dtrsBillingGroupName' - The name of the billing group the thing belongs to.
--
-- * 'dtrsThingId' - The ID of the thing to describe.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeThingResponse ::
  -- | 'dtrsResponseStatus'
  Int ->
  DescribeThingResponse
describeThingResponse pResponseStatus_ =
  DescribeThingResponse'
    { _dtrsDefaultClientId = Nothing,
      _dtrsThingTypeName = Nothing,
      _dtrsThingARN = Nothing,
      _dtrsAttributes = Nothing,
      _dtrsVersion = Nothing,
      _dtrsThingName = Nothing,
      _dtrsBillingGroupName = Nothing,
      _dtrsThingId = Nothing,
      _dtrsResponseStatus = pResponseStatus_
    }

-- | The default MQTT client ID. For a typical device, the thing name is also used as the default MQTT client ID. Although we don’t require a mapping between a thing's registry name and its use of MQTT client IDs, certificates, or shadow state, we recommend that you choose a thing name and use it as the MQTT client ID for the registry and the Device Shadow service. This lets you better organize your AWS IoT fleet without removing the flexibility of the underlying device certificate model or shadows.
dtrsDefaultClientId :: Lens' DescribeThingResponse (Maybe Text)
dtrsDefaultClientId = lens _dtrsDefaultClientId (\s a -> s {_dtrsDefaultClientId = a})

-- | The thing type name.
dtrsThingTypeName :: Lens' DescribeThingResponse (Maybe Text)
dtrsThingTypeName = lens _dtrsThingTypeName (\s a -> s {_dtrsThingTypeName = a})

-- | The ARN of the thing to describe.
dtrsThingARN :: Lens' DescribeThingResponse (Maybe Text)
dtrsThingARN = lens _dtrsThingARN (\s a -> s {_dtrsThingARN = a})

-- | The thing attributes.
dtrsAttributes :: Lens' DescribeThingResponse (HashMap Text (Text))
dtrsAttributes = lens _dtrsAttributes (\s a -> s {_dtrsAttributes = a}) . _Default . _Map

-- | The current version of the thing record in the registry.
dtrsVersion :: Lens' DescribeThingResponse (Maybe Integer)
dtrsVersion = lens _dtrsVersion (\s a -> s {_dtrsVersion = a})

-- | The name of the thing.
dtrsThingName :: Lens' DescribeThingResponse (Maybe Text)
dtrsThingName = lens _dtrsThingName (\s a -> s {_dtrsThingName = a})

-- | The name of the billing group the thing belongs to.
dtrsBillingGroupName :: Lens' DescribeThingResponse (Maybe Text)
dtrsBillingGroupName = lens _dtrsBillingGroupName (\s a -> s {_dtrsBillingGroupName = a})

-- | The ID of the thing to describe.
dtrsThingId :: Lens' DescribeThingResponse (Maybe Text)
dtrsThingId = lens _dtrsThingId (\s a -> s {_dtrsThingId = a})

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeThingResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\s a -> s {_dtrsResponseStatus = a})

instance NFData DescribeThingResponse
