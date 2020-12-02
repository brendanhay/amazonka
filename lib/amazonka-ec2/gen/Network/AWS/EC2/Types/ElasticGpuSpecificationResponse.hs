{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuSpecificationResponse where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an elastic GPU.
--
--
--
-- /See:/ 'elasticGpuSpecificationResponse' smart constructor.
newtype ElasticGpuSpecificationResponse = ElasticGpuSpecificationResponse'
  { _eType ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticGpuSpecificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eType' - The elastic GPU type.
elasticGpuSpecificationResponse ::
  ElasticGpuSpecificationResponse
elasticGpuSpecificationResponse =
  ElasticGpuSpecificationResponse' {_eType = Nothing}

-- | The elastic GPU type.
eType :: Lens' ElasticGpuSpecificationResponse (Maybe Text)
eType = lens _eType (\s a -> s {_eType = a})

instance FromXML ElasticGpuSpecificationResponse where
  parseXML x = ElasticGpuSpecificationResponse' <$> (x .@? "type")

instance Hashable ElasticGpuSpecificationResponse

instance NFData ElasticGpuSpecificationResponse
