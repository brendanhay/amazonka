{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ElasticIP where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Elastic IP address.
--
--
--
-- /See:/ 'elasticIP' smart constructor.
data ElasticIP = ElasticIP'
  { _eiInstanceId :: !(Maybe Text),
    _eiDomain :: !(Maybe Text),
    _eiIP :: !(Maybe Text),
    _eiName :: !(Maybe Text),
    _eiRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiInstanceId' - The ID of the instance that the address is attached to.
--
-- * 'eiDomain' - The domain.
--
-- * 'eiIP' - The IP address.
--
-- * 'eiName' - The name.
--
-- * 'eiRegion' - The AWS region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
elasticIP ::
  ElasticIP
elasticIP =
  ElasticIP'
    { _eiInstanceId = Nothing,
      _eiDomain = Nothing,
      _eiIP = Nothing,
      _eiName = Nothing,
      _eiRegion = Nothing
    }

-- | The ID of the instance that the address is attached to.
eiInstanceId :: Lens' ElasticIP (Maybe Text)
eiInstanceId = lens _eiInstanceId (\s a -> s {_eiInstanceId = a})

-- | The domain.
eiDomain :: Lens' ElasticIP (Maybe Text)
eiDomain = lens _eiDomain (\s a -> s {_eiDomain = a})

-- | The IP address.
eiIP :: Lens' ElasticIP (Maybe Text)
eiIP = lens _eiIP (\s a -> s {_eiIP = a})

-- | The name.
eiName :: Lens' ElasticIP (Maybe Text)
eiName = lens _eiName (\s a -> s {_eiName = a})

-- | The AWS region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
eiRegion :: Lens' ElasticIP (Maybe Text)
eiRegion = lens _eiRegion (\s a -> s {_eiRegion = a})

instance FromJSON ElasticIP where
  parseJSON =
    withObject
      "ElasticIP"
      ( \x ->
          ElasticIP'
            <$> (x .:? "InstanceId")
            <*> (x .:? "Domain")
            <*> (x .:? "Ip")
            <*> (x .:? "Name")
            <*> (x .:? "Region")
      )

instance Hashable ElasticIP

instance NFData ElasticIP
