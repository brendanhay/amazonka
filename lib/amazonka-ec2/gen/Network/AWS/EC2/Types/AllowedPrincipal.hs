{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllowedPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllowedPrincipal where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrincipalType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a principal.
--
--
--
-- /See:/ 'allowedPrincipal' smart constructor.
data AllowedPrincipal = AllowedPrincipal'
  { _apPrincipalType ::
      !(Maybe PrincipalType),
    _apPrincipal :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllowedPrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPrincipalType' - The type of principal.
--
-- * 'apPrincipal' - The Amazon Resource Name (ARN) of the principal.
allowedPrincipal ::
  AllowedPrincipal
allowedPrincipal =
  AllowedPrincipal'
    { _apPrincipalType = Nothing,
      _apPrincipal = Nothing
    }

-- | The type of principal.
apPrincipalType :: Lens' AllowedPrincipal (Maybe PrincipalType)
apPrincipalType = lens _apPrincipalType (\s a -> s {_apPrincipalType = a})

-- | The Amazon Resource Name (ARN) of the principal.
apPrincipal :: Lens' AllowedPrincipal (Maybe Text)
apPrincipal = lens _apPrincipal (\s a -> s {_apPrincipal = a})

instance FromXML AllowedPrincipal where
  parseXML x =
    AllowedPrincipal'
      <$> (x .@? "principalType") <*> (x .@? "principal")

instance Hashable AllowedPrincipal

instance NFData AllowedPrincipal
