{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an elastic inference accelerator.
--
--
--
-- /See:/ 'launchTemplateElasticInferenceAcceleratorResponse' smart constructor.
data LaunchTemplateElasticInferenceAcceleratorResponse = LaunchTemplateElasticInferenceAcceleratorResponse'
  { _lCount ::
      !( Maybe
           Int
       ),
    _lType ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateElasticInferenceAcceleratorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lCount' - The number of elastic inference accelerators to attach to the instance.  Default: 1
--
-- * 'lType' - The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
launchTemplateElasticInferenceAcceleratorResponse ::
  LaunchTemplateElasticInferenceAcceleratorResponse
launchTemplateElasticInferenceAcceleratorResponse =
  LaunchTemplateElasticInferenceAcceleratorResponse'
    { _lCount =
        Nothing,
      _lType = Nothing
    }

-- | The number of elastic inference accelerators to attach to the instance.  Default: 1
lCount :: Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Maybe Int)
lCount = lens _lCount (\s a -> s {_lCount = a})

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
lType :: Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Maybe Text)
lType = lens _lType (\s a -> s {_lType = a})

instance FromXML LaunchTemplateElasticInferenceAcceleratorResponse where
  parseXML x =
    LaunchTemplateElasticInferenceAcceleratorResponse'
      <$> (x .@? "count") <*> (x .@? "type")

instance Hashable LaunchTemplateElasticInferenceAcceleratorResponse

instance NFData LaunchTemplateElasticInferenceAcceleratorResponse
