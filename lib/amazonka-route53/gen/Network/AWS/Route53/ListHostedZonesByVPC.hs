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
-- Module      : Network.AWS.Route53.ListHostedZonesByVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the private hosted zones that a specified VPC is associated with, regardless of which AWS account or AWS service owns the hosted zones. The @HostedZoneOwner@ structure in the response contains one of the following values:
--
--
--     * An @OwningAccount@ element, which contains the account number of either the current AWS account or another AWS account. Some services, such as AWS Cloud Map, create hosted zones using the current account.
--
--     * An @OwningService@ element, which identifies the AWS service that created and owns the hosted zone. For example, if a hosted zone was created by Amazon Elastic File System (Amazon EFS), the value of @Owner@ is @efs.amazonaws.com@ .
module Network.AWS.Route53.ListHostedZonesByVPC
  ( -- * Creating a Request
    listHostedZonesByVPC,
    ListHostedZonesByVPC,

    -- * Request Lenses
    lhzbvNextToken,
    lhzbvMaxItems,
    lhzbvVPCId,
    lhzbvVPCRegion,

    -- * Destructuring the Response
    listHostedZonesByVPCResponse,
    ListHostedZonesByVPCResponse,

    -- * Response Lenses
    lhzbvrsNextToken,
    lhzbvrsResponseStatus,
    lhzbvrsHostedZoneSummaries,
    lhzbvrsMaxItems,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types

-- | Lists all the private hosted zones that a specified VPC is associated with, regardless of which AWS account created the hosted zones.
--
--
--
-- /See:/ 'listHostedZonesByVPC' smart constructor.
data ListHostedZonesByVPC = ListHostedZonesByVPC'
  { _lhzbvNextToken ::
      !(Maybe Text),
    _lhzbvMaxItems :: !(Maybe Text),
    _lhzbvVPCId :: !Text,
    _lhzbvVPCRegion :: !VPCRegion
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHostedZonesByVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzbvNextToken' - If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request.  For the value of @NextToken@ , specify the value of @NextToken@ from the previous response. If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
--
-- * 'lhzbvMaxItems' - (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
--
-- * 'lhzbvVPCId' - The ID of the Amazon VPC that you want to list hosted zones for.
--
-- * 'lhzbvVPCRegion' - For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in.
listHostedZonesByVPC ::
  -- | 'lhzbvVPCId'
  Text ->
  -- | 'lhzbvVPCRegion'
  VPCRegion ->
  ListHostedZonesByVPC
listHostedZonesByVPC pVPCId_ pVPCRegion_ =
  ListHostedZonesByVPC'
    { _lhzbvNextToken = Nothing,
      _lhzbvMaxItems = Nothing,
      _lhzbvVPCId = pVPCId_,
      _lhzbvVPCRegion = pVPCRegion_
    }

-- | If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request.  For the value of @NextToken@ , specify the value of @NextToken@ from the previous response. If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
lhzbvNextToken :: Lens' ListHostedZonesByVPC (Maybe Text)
lhzbvNextToken = lens _lhzbvNextToken (\s a -> s {_lhzbvNextToken = a})

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
lhzbvMaxItems :: Lens' ListHostedZonesByVPC (Maybe Text)
lhzbvMaxItems = lens _lhzbvMaxItems (\s a -> s {_lhzbvMaxItems = a})

-- | The ID of the Amazon VPC that you want to list hosted zones for.
lhzbvVPCId :: Lens' ListHostedZonesByVPC Text
lhzbvVPCId = lens _lhzbvVPCId (\s a -> s {_lhzbvVPCId = a})

-- | For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in.
lhzbvVPCRegion :: Lens' ListHostedZonesByVPC VPCRegion
lhzbvVPCRegion = lens _lhzbvVPCRegion (\s a -> s {_lhzbvVPCRegion = a})

instance AWSRequest ListHostedZonesByVPC where
  type Rs ListHostedZonesByVPC = ListHostedZonesByVPCResponse
  request = get route53
  response =
    receiveXML
      ( \s h x ->
          ListHostedZonesByVPCResponse'
            <$> (x .@? "NextToken")
            <*> (pure (fromEnum s))
            <*> ( x .@? "HostedZoneSummaries" .!@ mempty
                    >>= parseXMLList "HostedZoneSummary"
                )
            <*> (x .@ "MaxItems")
      )

instance Hashable ListHostedZonesByVPC

instance NFData ListHostedZonesByVPC

instance ToHeaders ListHostedZonesByVPC where
  toHeaders = const mempty

instance ToPath ListHostedZonesByVPC where
  toPath = const "/2013-04-01/hostedzonesbyvpc"

instance ToQuery ListHostedZonesByVPC where
  toQuery ListHostedZonesByVPC' {..} =
    mconcat
      [ "nexttoken" =: _lhzbvNextToken,
        "maxitems" =: _lhzbvMaxItems,
        "vpcid" =: _lhzbvVPCId,
        "vpcregion" =: _lhzbvVPCRegion
      ]

-- | /See:/ 'listHostedZonesByVPCResponse' smart constructor.
data ListHostedZonesByVPCResponse = ListHostedZonesByVPCResponse'
  { _lhzbvrsNextToken ::
      !(Maybe Text),
    _lhzbvrsResponseStatus :: !Int,
    _lhzbvrsHostedZoneSummaries ::
      ![HostedZoneSummary],
    _lhzbvrsMaxItems :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHostedZonesByVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzbvrsNextToken' - The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
--
-- * 'lhzbvrsResponseStatus' - -- | The response status code.
--
-- * 'lhzbvrsHostedZoneSummaries' - A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
-- * 'lhzbvrsMaxItems' - The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
listHostedZonesByVPCResponse ::
  -- | 'lhzbvrsResponseStatus'
  Int ->
  -- | 'lhzbvrsMaxItems'
  Text ->
  ListHostedZonesByVPCResponse
listHostedZonesByVPCResponse pResponseStatus_ pMaxItems_ =
  ListHostedZonesByVPCResponse'
    { _lhzbvrsNextToken = Nothing,
      _lhzbvrsResponseStatus = pResponseStatus_,
      _lhzbvrsHostedZoneSummaries = mempty,
      _lhzbvrsMaxItems = pMaxItems_
    }

-- | The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
lhzbvrsNextToken :: Lens' ListHostedZonesByVPCResponse (Maybe Text)
lhzbvrsNextToken = lens _lhzbvrsNextToken (\s a -> s {_lhzbvrsNextToken = a})

-- | -- | The response status code.
lhzbvrsResponseStatus :: Lens' ListHostedZonesByVPCResponse Int
lhzbvrsResponseStatus = lens _lhzbvrsResponseStatus (\s a -> s {_lhzbvrsResponseStatus = a})

-- | A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
lhzbvrsHostedZoneSummaries :: Lens' ListHostedZonesByVPCResponse [HostedZoneSummary]
lhzbvrsHostedZoneSummaries = lens _lhzbvrsHostedZoneSummaries (\s a -> s {_lhzbvrsHostedZoneSummaries = a}) . _Coerce

-- | The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
lhzbvrsMaxItems :: Lens' ListHostedZonesByVPCResponse Text
lhzbvrsMaxItems = lens _lhzbvrsMaxItems (\s a -> s {_lhzbvrsMaxItems = a})

instance NFData ListHostedZonesByVPCResponse
