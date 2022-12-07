{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.AccessControlAttribute

-- | Specifies the attributes to add to your attribute-based access control
-- (ABAC) configuration.
--
-- /See:/ 'newInstanceAccessControlAttributeConfiguration' smart constructor.
data InstanceAccessControlAttributeConfiguration = InstanceAccessControlAttributeConfiguration'
  { -- | Lists the attributes that are configured for ABAC in the specified IAM
    -- Identity Center instance.
    accessControlAttributes :: [AccessControlAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceAccessControlAttributeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlAttributes', 'instanceAccessControlAttributeConfiguration_accessControlAttributes' - Lists the attributes that are configured for ABAC in the specified IAM
-- Identity Center instance.
newInstanceAccessControlAttributeConfiguration ::
  InstanceAccessControlAttributeConfiguration
newInstanceAccessControlAttributeConfiguration =
  InstanceAccessControlAttributeConfiguration'
    { accessControlAttributes =
        Prelude.mempty
    }

-- | Lists the attributes that are configured for ABAC in the specified IAM
-- Identity Center instance.
instanceAccessControlAttributeConfiguration_accessControlAttributes :: Lens.Lens' InstanceAccessControlAttributeConfiguration [AccessControlAttribute]
instanceAccessControlAttributeConfiguration_accessControlAttributes = Lens.lens (\InstanceAccessControlAttributeConfiguration' {accessControlAttributes} -> accessControlAttributes) (\s@InstanceAccessControlAttributeConfiguration' {} a -> s {accessControlAttributes = a} :: InstanceAccessControlAttributeConfiguration) Prelude.. Lens.coerced

instance
  Data.FromJSON
    InstanceAccessControlAttributeConfiguration
  where
  parseJSON =
    Data.withObject
      "InstanceAccessControlAttributeConfiguration"
      ( \x ->
          InstanceAccessControlAttributeConfiguration'
            Prelude.<$> ( x Data..:? "AccessControlAttributes"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    InstanceAccessControlAttributeConfiguration
  where
  hashWithSalt
    _salt
    InstanceAccessControlAttributeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` accessControlAttributes

instance
  Prelude.NFData
    InstanceAccessControlAttributeConfiguration
  where
  rnf InstanceAccessControlAttributeConfiguration' {..} =
    Prelude.rnf accessControlAttributes

instance
  Data.ToJSON
    InstanceAccessControlAttributeConfiguration
  where
  toJSON
    InstanceAccessControlAttributeConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "AccessControlAttributes"
                    Data..= accessControlAttributes
                )
            ]
        )
