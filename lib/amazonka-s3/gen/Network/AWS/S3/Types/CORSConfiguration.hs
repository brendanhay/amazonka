{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CORSConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CORSConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CORSRule

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'corsConfiguration' smart constructor.
newtype CORSConfiguration = CORSConfiguration'
  { _ccCORSRules ::
      [CORSRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CORSConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCORSRules' - A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
corsConfiguration ::
  CORSConfiguration
corsConfiguration = CORSConfiguration' {_ccCORSRules = mempty}

-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
ccCORSRules :: Lens' CORSConfiguration [CORSRule]
ccCORSRules = lens _ccCORSRules (\s a -> s {_ccCORSRules = a}) . _Coerce

instance Hashable CORSConfiguration

instance NFData CORSConfiguration

instance ToXML CORSConfiguration where
  toXML CORSConfiguration' {..} =
    mconcat [toXMLList "CORSRule" _ccCORSRules]
