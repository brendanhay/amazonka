{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.Principal where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.PrincipalType

-- | Information about a principal.
--
--
--
-- /See:/ 'principal' smart constructor.
data Principal = Principal'
  { _pPrincipalType ::
      !(Maybe PrincipalType),
    _pPrincipalARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Principal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPrincipalType' - The principal type. The supported value is @IAM@ .
--
-- * 'pPrincipalARN' - The ARN of the principal (IAM user, role, or group).
principal ::
  Principal
principal =
  Principal' {_pPrincipalType = Nothing, _pPrincipalARN = Nothing}

-- | The principal type. The supported value is @IAM@ .
pPrincipalType :: Lens' Principal (Maybe PrincipalType)
pPrincipalType = lens _pPrincipalType (\s a -> s {_pPrincipalType = a})

-- | The ARN of the principal (IAM user, role, or group).
pPrincipalARN :: Lens' Principal (Maybe Text)
pPrincipalARN = lens _pPrincipalARN (\s a -> s {_pPrincipalARN = a})

instance FromJSON Principal where
  parseJSON =
    withObject
      "Principal"
      ( \x ->
          Principal' <$> (x .:? "PrincipalType") <*> (x .:? "PrincipalARN")
      )

instance Hashable Principal

instance NFData Principal
