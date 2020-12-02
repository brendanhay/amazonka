{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Protection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Protection where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents a resource that is under DDoS protection.
--
--
--
-- /See:/ 'protection' smart constructor.
data Protection = Protection'
  { _pHealthCheckIds :: !(Maybe [Text]),
    _pResourceARN :: !(Maybe Text),
    _pName :: !(Maybe Text),
    _pId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Protection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pHealthCheckIds' - The unique identifier (ID) for the Route 53 health check that's associated with the protection.
--
-- * 'pResourceARN' - The ARN (Amazon Resource Name) of the AWS resource that is protected.
--
-- * 'pName' - The name of the protection. For example, @My CloudFront distributions@ .
--
-- * 'pId' - The unique identifier (ID) of the protection.
protection ::
  Protection
protection =
  Protection'
    { _pHealthCheckIds = Nothing,
      _pResourceARN = Nothing,
      _pName = Nothing,
      _pId = Nothing
    }

-- | The unique identifier (ID) for the Route 53 health check that's associated with the protection.
pHealthCheckIds :: Lens' Protection [Text]
pHealthCheckIds = lens _pHealthCheckIds (\s a -> s {_pHealthCheckIds = a}) . _Default . _Coerce

-- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
pResourceARN :: Lens' Protection (Maybe Text)
pResourceARN = lens _pResourceARN (\s a -> s {_pResourceARN = a})

-- | The name of the protection. For example, @My CloudFront distributions@ .
pName :: Lens' Protection (Maybe Text)
pName = lens _pName (\s a -> s {_pName = a})

-- | The unique identifier (ID) of the protection.
pId :: Lens' Protection (Maybe Text)
pId = lens _pId (\s a -> s {_pId = a})

instance FromJSON Protection where
  parseJSON =
    withObject
      "Protection"
      ( \x ->
          Protection'
            <$> (x .:? "HealthCheckIds" .!= mempty)
            <*> (x .:? "ResourceArn")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
      )

instance Hashable Protection

instance NFData Protection
