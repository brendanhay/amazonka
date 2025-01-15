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
-- Module      : Amazonka.CloudFormation.Types.RequiredActivatedType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.RequiredActivatedType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For extensions that are modules, a public third-party extension that
-- must be activated in your account in order for the module itself to be
-- activated.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/modules.html#module-enabling Activating public modules for use in your account>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newRequiredActivatedType' smart constructor.
data RequiredActivatedType = RequiredActivatedType'
  { -- | The type name of the public extension.
    --
    -- If you specified a @TypeNameAlias@ when enabling the extension in this
    -- account and region, CloudFormation treats that alias as the extension\'s
    -- type name within the account and region, not the type name of the public
    -- extension. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
    -- in the /CloudFormation User Guide/.
    originalTypeName :: Prelude.Maybe Prelude.Text,
    -- | The publisher ID of the extension publisher.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | A list of the major versions of the extension type that the macro
    -- supports.
    supportedMajorVersions :: Prelude.Maybe [Prelude.Natural],
    -- | An alias assigned to the public extension, in this account and region.
    -- If you specify an alias for the extension, CloudFormation treats the
    -- alias as the extension type name within this account and region. You
    -- must use the alias to refer to the extension in your templates, API
    -- calls, and CloudFormation console.
    typeNameAlias :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequiredActivatedType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originalTypeName', 'requiredActivatedType_originalTypeName' - The type name of the public extension.
--
-- If you specified a @TypeNameAlias@ when enabling the extension in this
-- account and region, CloudFormation treats that alias as the extension\'s
-- type name within the account and region, not the type name of the public
-- extension. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
-- in the /CloudFormation User Guide/.
--
-- 'publisherId', 'requiredActivatedType_publisherId' - The publisher ID of the extension publisher.
--
-- 'supportedMajorVersions', 'requiredActivatedType_supportedMajorVersions' - A list of the major versions of the extension type that the macro
-- supports.
--
-- 'typeNameAlias', 'requiredActivatedType_typeNameAlias' - An alias assigned to the public extension, in this account and region.
-- If you specify an alias for the extension, CloudFormation treats the
-- alias as the extension type name within this account and region. You
-- must use the alias to refer to the extension in your templates, API
-- calls, and CloudFormation console.
newRequiredActivatedType ::
  RequiredActivatedType
newRequiredActivatedType =
  RequiredActivatedType'
    { originalTypeName =
        Prelude.Nothing,
      publisherId = Prelude.Nothing,
      supportedMajorVersions = Prelude.Nothing,
      typeNameAlias = Prelude.Nothing
    }

-- | The type name of the public extension.
--
-- If you specified a @TypeNameAlias@ when enabling the extension in this
-- account and region, CloudFormation treats that alias as the extension\'s
-- type name within the account and region, not the type name of the public
-- extension. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
-- in the /CloudFormation User Guide/.
requiredActivatedType_originalTypeName :: Lens.Lens' RequiredActivatedType (Prelude.Maybe Prelude.Text)
requiredActivatedType_originalTypeName = Lens.lens (\RequiredActivatedType' {originalTypeName} -> originalTypeName) (\s@RequiredActivatedType' {} a -> s {originalTypeName = a} :: RequiredActivatedType)

-- | The publisher ID of the extension publisher.
requiredActivatedType_publisherId :: Lens.Lens' RequiredActivatedType (Prelude.Maybe Prelude.Text)
requiredActivatedType_publisherId = Lens.lens (\RequiredActivatedType' {publisherId} -> publisherId) (\s@RequiredActivatedType' {} a -> s {publisherId = a} :: RequiredActivatedType)

-- | A list of the major versions of the extension type that the macro
-- supports.
requiredActivatedType_supportedMajorVersions :: Lens.Lens' RequiredActivatedType (Prelude.Maybe [Prelude.Natural])
requiredActivatedType_supportedMajorVersions = Lens.lens (\RequiredActivatedType' {supportedMajorVersions} -> supportedMajorVersions) (\s@RequiredActivatedType' {} a -> s {supportedMajorVersions = a} :: RequiredActivatedType) Prelude.. Lens.mapping Lens.coerced

-- | An alias assigned to the public extension, in this account and region.
-- If you specify an alias for the extension, CloudFormation treats the
-- alias as the extension type name within this account and region. You
-- must use the alias to refer to the extension in your templates, API
-- calls, and CloudFormation console.
requiredActivatedType_typeNameAlias :: Lens.Lens' RequiredActivatedType (Prelude.Maybe Prelude.Text)
requiredActivatedType_typeNameAlias = Lens.lens (\RequiredActivatedType' {typeNameAlias} -> typeNameAlias) (\s@RequiredActivatedType' {} a -> s {typeNameAlias = a} :: RequiredActivatedType)

instance Data.FromXML RequiredActivatedType where
  parseXML x =
    RequiredActivatedType'
      Prelude.<$> (x Data..@? "OriginalTypeName")
      Prelude.<*> (x Data..@? "PublisherId")
      Prelude.<*> ( x
                      Data..@? "SupportedMajorVersions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "TypeNameAlias")

instance Prelude.Hashable RequiredActivatedType where
  hashWithSalt _salt RequiredActivatedType' {..} =
    _salt
      `Prelude.hashWithSalt` originalTypeName
      `Prelude.hashWithSalt` publisherId
      `Prelude.hashWithSalt` supportedMajorVersions
      `Prelude.hashWithSalt` typeNameAlias

instance Prelude.NFData RequiredActivatedType where
  rnf RequiredActivatedType' {..} =
    Prelude.rnf originalTypeName `Prelude.seq`
      Prelude.rnf publisherId `Prelude.seq`
        Prelude.rnf supportedMajorVersions `Prelude.seq`
          Prelude.rnf typeNameAlias
