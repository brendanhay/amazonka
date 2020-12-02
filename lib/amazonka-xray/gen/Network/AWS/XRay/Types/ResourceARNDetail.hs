{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResourceARNDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResourceARNDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of resources ARNs corresponding to the segments in a trace.
--
--
--
-- /See:/ 'resourceARNDetail' smart constructor.
newtype ResourceARNDetail = ResourceARNDetail'
  { _radARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceARNDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'radARN' - The ARN of a corresponding resource.
resourceARNDetail ::
  ResourceARNDetail
resourceARNDetail = ResourceARNDetail' {_radARN = Nothing}

-- | The ARN of a corresponding resource.
radARN :: Lens' ResourceARNDetail (Maybe Text)
radARN = lens _radARN (\s a -> s {_radARN = a})

instance FromJSON ResourceARNDetail where
  parseJSON =
    withObject
      "ResourceARNDetail"
      (\x -> ResourceARNDetail' <$> (x .:? "ARN"))

instance Hashable ResourceARNDetail

instance NFData ResourceARNDetail
