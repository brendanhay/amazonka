{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HibernationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HibernationOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether your instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'hibernationOptions' smart constructor.
newtype HibernationOptions = HibernationOptions'
  { _hoConfigured ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HibernationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoConfigured' - If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
hibernationOptions ::
  HibernationOptions
hibernationOptions = HibernationOptions' {_hoConfigured = Nothing}

-- | If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
hoConfigured :: Lens' HibernationOptions (Maybe Bool)
hoConfigured = lens _hoConfigured (\s a -> s {_hoConfigured = a})

instance FromXML HibernationOptions where
  parseXML x = HibernationOptions' <$> (x .@? "configured")

instance Hashable HibernationOptions

instance NFData HibernationOptions
