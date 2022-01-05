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
-- Module      : Amazonka.CloudFormation.Types.TypeConfigurationIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.TypeConfigurationIdentifier where

import Amazonka.CloudFormation.Types.ThirdPartyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Identifying information for the configuration of a CloudFormation
-- extension.
--
-- /See:/ 'newTypeConfigurationIdentifier' smart constructor.
data TypeConfigurationIdentifier = TypeConfigurationIdentifier'
  { -- | The name of the extension type to which this configuration applies.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the configuration, in this account
    -- and region.
    typeConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The alias specified for this configuration, if one was specified when
    -- the configuration was set.
    typeConfigurationAlias :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the extension, in this account and
    -- region.
    --
    -- For public extensions, this will be the ARN assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
    -- in this account and region. For private extensions, this will be the ARN
    -- assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
    -- in this account and region.
    typeArn :: Prelude.Maybe Prelude.Text,
    -- | The type of extension.
    type' :: Prelude.Maybe ThirdPartyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypeConfigurationIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'typeConfigurationIdentifier_typeName' - The name of the extension type to which this configuration applies.
--
-- 'typeConfigurationArn', 'typeConfigurationIdentifier_typeConfigurationArn' - The Amazon Resource Name (ARN) for the configuration, in this account
-- and region.
--
-- 'typeConfigurationAlias', 'typeConfigurationIdentifier_typeConfigurationAlias' - The alias specified for this configuration, if one was specified when
-- the configuration was set.
--
-- 'typeArn', 'typeConfigurationIdentifier_typeArn' - The Amazon Resource Name (ARN) for the extension, in this account and
-- region.
--
-- For public extensions, this will be the ARN assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
-- in this account and region. For private extensions, this will be the ARN
-- assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
-- in this account and region.
--
-- 'type'', 'typeConfigurationIdentifier_type' - The type of extension.
newTypeConfigurationIdentifier ::
  TypeConfigurationIdentifier
newTypeConfigurationIdentifier =
  TypeConfigurationIdentifier'
    { typeName =
        Prelude.Nothing,
      typeConfigurationArn = Prelude.Nothing,
      typeConfigurationAlias = Prelude.Nothing,
      typeArn = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the extension type to which this configuration applies.
typeConfigurationIdentifier_typeName :: Lens.Lens' TypeConfigurationIdentifier (Prelude.Maybe Prelude.Text)
typeConfigurationIdentifier_typeName = Lens.lens (\TypeConfigurationIdentifier' {typeName} -> typeName) (\s@TypeConfigurationIdentifier' {} a -> s {typeName = a} :: TypeConfigurationIdentifier)

-- | The Amazon Resource Name (ARN) for the configuration, in this account
-- and region.
typeConfigurationIdentifier_typeConfigurationArn :: Lens.Lens' TypeConfigurationIdentifier (Prelude.Maybe Prelude.Text)
typeConfigurationIdentifier_typeConfigurationArn = Lens.lens (\TypeConfigurationIdentifier' {typeConfigurationArn} -> typeConfigurationArn) (\s@TypeConfigurationIdentifier' {} a -> s {typeConfigurationArn = a} :: TypeConfigurationIdentifier)

-- | The alias specified for this configuration, if one was specified when
-- the configuration was set.
typeConfigurationIdentifier_typeConfigurationAlias :: Lens.Lens' TypeConfigurationIdentifier (Prelude.Maybe Prelude.Text)
typeConfigurationIdentifier_typeConfigurationAlias = Lens.lens (\TypeConfigurationIdentifier' {typeConfigurationAlias} -> typeConfigurationAlias) (\s@TypeConfigurationIdentifier' {} a -> s {typeConfigurationAlias = a} :: TypeConfigurationIdentifier)

-- | The Amazon Resource Name (ARN) for the extension, in this account and
-- region.
--
-- For public extensions, this will be the ARN assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
-- in this account and region. For private extensions, this will be the ARN
-- assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
-- in this account and region.
typeConfigurationIdentifier_typeArn :: Lens.Lens' TypeConfigurationIdentifier (Prelude.Maybe Prelude.Text)
typeConfigurationIdentifier_typeArn = Lens.lens (\TypeConfigurationIdentifier' {typeArn} -> typeArn) (\s@TypeConfigurationIdentifier' {} a -> s {typeArn = a} :: TypeConfigurationIdentifier)

-- | The type of extension.
typeConfigurationIdentifier_type :: Lens.Lens' TypeConfigurationIdentifier (Prelude.Maybe ThirdPartyType)
typeConfigurationIdentifier_type = Lens.lens (\TypeConfigurationIdentifier' {type'} -> type') (\s@TypeConfigurationIdentifier' {} a -> s {type' = a} :: TypeConfigurationIdentifier)

instance Core.FromXML TypeConfigurationIdentifier where
  parseXML x =
    TypeConfigurationIdentifier'
      Prelude.<$> (x Core..@? "TypeName")
      Prelude.<*> (x Core..@? "TypeConfigurationArn")
      Prelude.<*> (x Core..@? "TypeConfigurationAlias")
      Prelude.<*> (x Core..@? "TypeArn")
      Prelude.<*> (x Core..@? "Type")

instance Prelude.Hashable TypeConfigurationIdentifier where
  hashWithSalt _salt TypeConfigurationIdentifier' {..} =
    _salt `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` typeConfigurationArn
      `Prelude.hashWithSalt` typeConfigurationAlias
      `Prelude.hashWithSalt` typeArn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TypeConfigurationIdentifier where
  rnf TypeConfigurationIdentifier' {..} =
    Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf typeConfigurationArn
      `Prelude.seq` Prelude.rnf typeConfigurationAlias
      `Prelude.seq` Prelude.rnf typeArn
      `Prelude.seq` Prelude.rnf type'

instance Core.ToQuery TypeConfigurationIdentifier where
  toQuery TypeConfigurationIdentifier' {..} =
    Prelude.mconcat
      [ "TypeName" Core.=: typeName,
        "TypeConfigurationArn" Core.=: typeConfigurationArn,
        "TypeConfigurationAlias"
          Core.=: typeConfigurationAlias,
        "TypeArn" Core.=: typeArn,
        "Type" Core.=: type'
      ]
