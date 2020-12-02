{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HibernationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HibernationOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether your instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'hibernationOptionsRequest' smart constructor.
newtype HibernationOptionsRequest = HibernationOptionsRequest'
  { _horConfigured ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HibernationOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'horConfigured' - If you set this parameter to @true@ , your instance is enabled for hibernation. Default: @false@
hibernationOptionsRequest ::
  HibernationOptionsRequest
hibernationOptionsRequest =
  HibernationOptionsRequest' {_horConfigured = Nothing}

-- | If you set this parameter to @true@ , your instance is enabled for hibernation. Default: @false@
horConfigured :: Lens' HibernationOptionsRequest (Maybe Bool)
horConfigured = lens _horConfigured (\s a -> s {_horConfigured = a})

instance Hashable HibernationOptionsRequest

instance NFData HibernationOptionsRequest

instance ToQuery HibernationOptionsRequest where
  toQuery HibernationOptionsRequest' {..} =
    mconcat ["Configured" =: _horConfigured]
