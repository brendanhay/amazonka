{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputLocation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.S3OutputLocation

-- | An S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 'instanceAssociationOutputLocation' smart constructor.
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { _iaolS3Location ::
      Maybe
        S3OutputLocation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAssociationOutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaolS3Location' - An S3 bucket where you want to store the results of this request.
instanceAssociationOutputLocation ::
  InstanceAssociationOutputLocation
instanceAssociationOutputLocation =
  InstanceAssociationOutputLocation' {_iaolS3Location = Nothing}

-- | An S3 bucket where you want to store the results of this request.
iaolS3Location :: Lens' InstanceAssociationOutputLocation (Maybe S3OutputLocation)
iaolS3Location = lens _iaolS3Location (\s a -> s {_iaolS3Location = a})

instance FromJSON InstanceAssociationOutputLocation where
  parseJSON =
    withObject
      "InstanceAssociationOutputLocation"
      ( \x ->
          InstanceAssociationOutputLocation' <$> (x .:? "S3Location")
      )

instance Hashable InstanceAssociationOutputLocation

instance NFData InstanceAssociationOutputLocation

instance ToJSON InstanceAssociationOutputLocation where
  toJSON InstanceAssociationOutputLocation' {..} =
    object (catMaybes [("S3Location" .=) <$> _iaolS3Location])
