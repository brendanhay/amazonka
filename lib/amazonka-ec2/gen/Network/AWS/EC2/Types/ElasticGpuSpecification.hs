{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A specification for an Elastic Graphics accelerator.
--
--
--
-- /See:/ 'elasticGpuSpecification' smart constructor.
newtype ElasticGpuSpecification = ElasticGpuSpecification'
  { _egsType ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticGpuSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egsType' - The type of Elastic Graphics accelerator. For more information about the values to specify for @Type@ , see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html#elastic-graphics-basics Elastic Graphics Basics> , specifically the Elastic Graphics accelerator column, in the /Amazon Elastic Compute Cloud User Guide for Windows Instances/ .
elasticGpuSpecification ::
  -- | 'egsType'
  Text ->
  ElasticGpuSpecification
elasticGpuSpecification pType_ =
  ElasticGpuSpecification' {_egsType = pType_}

-- | The type of Elastic Graphics accelerator. For more information about the values to specify for @Type@ , see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html#elastic-graphics-basics Elastic Graphics Basics> , specifically the Elastic Graphics accelerator column, in the /Amazon Elastic Compute Cloud User Guide for Windows Instances/ .
egsType :: Lens' ElasticGpuSpecification Text
egsType = lens _egsType (\s a -> s {_egsType = a})

instance Hashable ElasticGpuSpecification

instance NFData ElasticGpuSpecification

instance ToQuery ElasticGpuSpecification where
  toQuery ElasticGpuSpecification' {..} = mconcat ["Type" =: _egsType]
