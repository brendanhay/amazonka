{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputURL where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.S3OutputURL

-- | The URL of S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 'instanceAssociationOutputURL' smart constructor.
newtype InstanceAssociationOutputURL = InstanceAssociationOutputURL'
  { _iaouS3OutputURL ::
      Maybe S3OutputURL
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAssociationOutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaouS3OutputURL' - The URL of S3 bucket where you want to store the results of this request.
instanceAssociationOutputURL ::
  InstanceAssociationOutputURL
instanceAssociationOutputURL =
  InstanceAssociationOutputURL' {_iaouS3OutputURL = Nothing}

-- | The URL of S3 bucket where you want to store the results of this request.
iaouS3OutputURL :: Lens' InstanceAssociationOutputURL (Maybe S3OutputURL)
iaouS3OutputURL = lens _iaouS3OutputURL (\s a -> s {_iaouS3OutputURL = a})

instance FromJSON InstanceAssociationOutputURL where
  parseJSON =
    withObject
      "InstanceAssociationOutputURL"
      (\x -> InstanceAssociationOutputURL' <$> (x .:? "S3OutputUrl"))

instance Hashable InstanceAssociationOutputURL

instance NFData InstanceAssociationOutputURL
