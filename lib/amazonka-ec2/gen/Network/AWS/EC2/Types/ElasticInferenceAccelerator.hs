{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAccelerator where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an elastic inference accelerator.
--
--
--
-- /See:/ 'elasticInferenceAccelerator' smart constructor.
data ElasticInferenceAccelerator = ElasticInferenceAccelerator'
  { _eiaCount ::
      !(Maybe Nat),
    _eiaType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticInferenceAccelerator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiaCount' - The number of elastic inference accelerators to attach to the instance.  Default: 1
--
-- * 'eiaType' - The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ .
elasticInferenceAccelerator ::
  -- | 'eiaType'
  Text ->
  ElasticInferenceAccelerator
elasticInferenceAccelerator pType_ =
  ElasticInferenceAccelerator'
    { _eiaCount = Nothing,
      _eiaType = pType_
    }

-- | The number of elastic inference accelerators to attach to the instance.  Default: 1
eiaCount :: Lens' ElasticInferenceAccelerator (Maybe Natural)
eiaCount = lens _eiaCount (\s a -> s {_eiaCount = a}) . mapping _Nat

-- | The type of elastic inference accelerator. The possible values are @eia1.medium@ , @eia1.large@ , @eia1.xlarge@ , @eia2.medium@ , @eia2.large@ , and @eia2.xlarge@ .
eiaType :: Lens' ElasticInferenceAccelerator Text
eiaType = lens _eiaType (\s a -> s {_eiaType = a})

instance Hashable ElasticInferenceAccelerator

instance NFData ElasticInferenceAccelerator

instance ToQuery ElasticInferenceAccelerator where
  toQuery ElasticInferenceAccelerator' {..} =
    mconcat ["Count" =: _eiaCount, "Type" =: _eiaType]
