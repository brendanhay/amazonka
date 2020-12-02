{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.HealthStatus

-- | In a response to a <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances> request, @HttpInstanceSummary@ contains information about one instance that matches the values that you specified in the request.
--
--
--
-- /See:/ 'hTTPInstanceSummary' smart constructor.
data HTTPInstanceSummary = HTTPInstanceSummary'
  { _httpisInstanceId ::
      !(Maybe Text),
    _httpisNamespaceName :: !(Maybe Text),
    _httpisAttributes :: !(Maybe (Map Text (Text))),
    _httpisServiceName :: !(Maybe Text),
    _httpisHealthStatus :: !(Maybe HealthStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPInstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpisInstanceId' - The ID of an instance that matches the values that you specified in the request.
--
-- * 'httpisNamespaceName' - The name of the namespace that you specified when you registered the instance.
--
-- * 'httpisAttributes' - If you included any attributes when you registered the instance, the values of those attributes.
--
-- * 'httpisServiceName' - The name of the service that you specified when you registered the instance.
--
-- * 'httpisHealthStatus' - If you configured health checking in the service, the current health status of the service instance.
hTTPInstanceSummary ::
  HTTPInstanceSummary
hTTPInstanceSummary =
  HTTPInstanceSummary'
    { _httpisInstanceId = Nothing,
      _httpisNamespaceName = Nothing,
      _httpisAttributes = Nothing,
      _httpisServiceName = Nothing,
      _httpisHealthStatus = Nothing
    }

-- | The ID of an instance that matches the values that you specified in the request.
httpisInstanceId :: Lens' HTTPInstanceSummary (Maybe Text)
httpisInstanceId = lens _httpisInstanceId (\s a -> s {_httpisInstanceId = a})

-- | The name of the namespace that you specified when you registered the instance.
httpisNamespaceName :: Lens' HTTPInstanceSummary (Maybe Text)
httpisNamespaceName = lens _httpisNamespaceName (\s a -> s {_httpisNamespaceName = a})

-- | If you included any attributes when you registered the instance, the values of those attributes.
httpisAttributes :: Lens' HTTPInstanceSummary (HashMap Text (Text))
httpisAttributes = lens _httpisAttributes (\s a -> s {_httpisAttributes = a}) . _Default . _Map

-- | The name of the service that you specified when you registered the instance.
httpisServiceName :: Lens' HTTPInstanceSummary (Maybe Text)
httpisServiceName = lens _httpisServiceName (\s a -> s {_httpisServiceName = a})

-- | If you configured health checking in the service, the current health status of the service instance.
httpisHealthStatus :: Lens' HTTPInstanceSummary (Maybe HealthStatus)
httpisHealthStatus = lens _httpisHealthStatus (\s a -> s {_httpisHealthStatus = a})

instance FromJSON HTTPInstanceSummary where
  parseJSON =
    withObject
      "HTTPInstanceSummary"
      ( \x ->
          HTTPInstanceSummary'
            <$> (x .:? "InstanceId")
            <*> (x .:? "NamespaceName")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "ServiceName")
            <*> (x .:? "HealthStatus")
      )

instance Hashable HTTPInstanceSummary

instance NFData HTTPInstanceSummary
