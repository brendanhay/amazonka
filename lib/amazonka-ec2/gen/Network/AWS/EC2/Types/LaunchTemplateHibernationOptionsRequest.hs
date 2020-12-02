{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> .
--
--
--
-- /See:/ 'launchTemplateHibernationOptionsRequest' smart constructor.
newtype LaunchTemplateHibernationOptionsRequest = LaunchTemplateHibernationOptionsRequest'
  { _lthorConfigured ::
      Maybe
        Bool
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateHibernationOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lthorConfigured' - If you set this parameter to @true@ , the instance is enabled for hibernation. Default: @false@
launchTemplateHibernationOptionsRequest ::
  LaunchTemplateHibernationOptionsRequest
launchTemplateHibernationOptionsRequest =
  LaunchTemplateHibernationOptionsRequest'
    { _lthorConfigured =
        Nothing
    }

-- | If you set this parameter to @true@ , the instance is enabled for hibernation. Default: @false@
lthorConfigured :: Lens' LaunchTemplateHibernationOptionsRequest (Maybe Bool)
lthorConfigured = lens _lthorConfigured (\s a -> s {_lthorConfigured = a})

instance Hashable LaunchTemplateHibernationOptionsRequest

instance NFData LaunchTemplateHibernationOptionsRequest

instance ToQuery LaunchTemplateHibernationOptionsRequest where
  toQuery LaunchTemplateHibernationOptionsRequest' {..} =
    mconcat ["Configured" =: _lthorConfigured]
