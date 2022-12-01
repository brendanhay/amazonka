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
-- Module      : Amazonka.Kendra.Types.OneDriveConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.OneDriveConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.OneDriveUsers
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to OneDrive as your
-- data source.
--
-- /See:/ 'newOneDriveConfiguration' smart constructor.
data OneDriveConfiguration = OneDriveConfiguration'
  { -- | A list of regular expression patterns to include certain documents in
    -- your OneDrive. Documents that match the patterns are included in the
    -- index. Documents that don\'t match the patterns are excluded from the
    -- index. If a document matches both an inclusion and exclusion pattern,
    -- the exclusion pattern takes precedence and the document isn\'t included
    -- in the index.
    --
    -- The pattern is applied to the file name.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map OneDrive data
    -- source attributes or field names to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to
    -- OneDrive fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The OneDrive data source field names must exist in your OneDrive custom
    -- metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | @TRUE@ to disable local groups information.
    disableLocalGroups :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to exclude certain documents in
    -- your OneDrive. Documents that match the patterns are excluded from the
    -- index. Documents that don\'t match the patterns are included in the
    -- index. If a document matches both an inclusion and exclusion pattern,
    -- the exclusion pattern takes precedence and the document isn\'t included
    -- in the index.
    --
    -- The pattern is applied to the file name.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The Azure Active Directory domain of the organization.
    tenantDomain :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
    -- the user name and password to connect to OneDrive. The user namd should
    -- be the application ID for the OneDrive application, and the password is
    -- the application key for the OneDrive application.
    secretArn :: Prelude.Text,
    -- | A list of user accounts whose documents should be indexed.
    oneDriveUsers :: OneDriveUsers
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OneDriveConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inclusionPatterns', 'oneDriveConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain documents in
-- your OneDrive. Documents that match the patterns are included in the
-- index. Documents that don\'t match the patterns are excluded from the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the file name.
--
-- 'fieldMappings', 'oneDriveConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map OneDrive data
-- source attributes or field names to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to
-- OneDrive fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The OneDrive data source field names must exist in your OneDrive custom
-- metadata.
--
-- 'disableLocalGroups', 'oneDriveConfiguration_disableLocalGroups' - @TRUE@ to disable local groups information.
--
-- 'exclusionPatterns', 'oneDriveConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain documents in
-- your OneDrive. Documents that match the patterns are excluded from the
-- index. Documents that don\'t match the patterns are included in the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the file name.
--
-- 'tenantDomain', 'oneDriveConfiguration_tenantDomain' - The Azure Active Directory domain of the organization.
--
-- 'secretArn', 'oneDriveConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
-- the user name and password to connect to OneDrive. The user namd should
-- be the application ID for the OneDrive application, and the password is
-- the application key for the OneDrive application.
--
-- 'oneDriveUsers', 'oneDriveConfiguration_oneDriveUsers' - A list of user accounts whose documents should be indexed.
newOneDriveConfiguration ::
  -- | 'tenantDomain'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'oneDriveUsers'
  OneDriveUsers ->
  OneDriveConfiguration
newOneDriveConfiguration
  pTenantDomain_
  pSecretArn_
  pOneDriveUsers_ =
    OneDriveConfiguration'
      { inclusionPatterns =
          Prelude.Nothing,
        fieldMappings = Prelude.Nothing,
        disableLocalGroups = Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        tenantDomain = pTenantDomain_,
        secretArn = pSecretArn_,
        oneDriveUsers = pOneDriveUsers_
      }

-- | A list of regular expression patterns to include certain documents in
-- your OneDrive. Documents that match the patterns are included in the
-- index. Documents that don\'t match the patterns are excluded from the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the file name.
oneDriveConfiguration_inclusionPatterns :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe [Prelude.Text])
oneDriveConfiguration_inclusionPatterns = Lens.lens (\OneDriveConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@OneDriveConfiguration' {} a -> s {inclusionPatterns = a} :: OneDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map OneDrive data
-- source attributes or field names to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to
-- OneDrive fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The OneDrive data source field names must exist in your OneDrive custom
-- metadata.
oneDriveConfiguration_fieldMappings :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
oneDriveConfiguration_fieldMappings = Lens.lens (\OneDriveConfiguration' {fieldMappings} -> fieldMappings) (\s@OneDriveConfiguration' {} a -> s {fieldMappings = a} :: OneDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to disable local groups information.
oneDriveConfiguration_disableLocalGroups :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe Prelude.Bool)
oneDriveConfiguration_disableLocalGroups = Lens.lens (\OneDriveConfiguration' {disableLocalGroups} -> disableLocalGroups) (\s@OneDriveConfiguration' {} a -> s {disableLocalGroups = a} :: OneDriveConfiguration)

-- | A list of regular expression patterns to exclude certain documents in
-- your OneDrive. Documents that match the patterns are excluded from the
-- index. Documents that don\'t match the patterns are included in the
-- index. If a document matches both an inclusion and exclusion pattern,
-- the exclusion pattern takes precedence and the document isn\'t included
-- in the index.
--
-- The pattern is applied to the file name.
oneDriveConfiguration_exclusionPatterns :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe [Prelude.Text])
oneDriveConfiguration_exclusionPatterns = Lens.lens (\OneDriveConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@OneDriveConfiguration' {} a -> s {exclusionPatterns = a} :: OneDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Azure Active Directory domain of the organization.
oneDriveConfiguration_tenantDomain :: Lens.Lens' OneDriveConfiguration Prelude.Text
oneDriveConfiguration_tenantDomain = Lens.lens (\OneDriveConfiguration' {tenantDomain} -> tenantDomain) (\s@OneDriveConfiguration' {} a -> s {tenantDomain = a} :: OneDriveConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Managersecret that contains
-- the user name and password to connect to OneDrive. The user namd should
-- be the application ID for the OneDrive application, and the password is
-- the application key for the OneDrive application.
oneDriveConfiguration_secretArn :: Lens.Lens' OneDriveConfiguration Prelude.Text
oneDriveConfiguration_secretArn = Lens.lens (\OneDriveConfiguration' {secretArn} -> secretArn) (\s@OneDriveConfiguration' {} a -> s {secretArn = a} :: OneDriveConfiguration)

-- | A list of user accounts whose documents should be indexed.
oneDriveConfiguration_oneDriveUsers :: Lens.Lens' OneDriveConfiguration OneDriveUsers
oneDriveConfiguration_oneDriveUsers = Lens.lens (\OneDriveConfiguration' {oneDriveUsers} -> oneDriveUsers) (\s@OneDriveConfiguration' {} a -> s {oneDriveUsers = a} :: OneDriveConfiguration)

instance Core.FromJSON OneDriveConfiguration where
  parseJSON =
    Core.withObject
      "OneDriveConfiguration"
      ( \x ->
          OneDriveConfiguration'
            Prelude.<$> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FieldMappings")
            Prelude.<*> (x Core..:? "DisableLocalGroups")
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "TenantDomain")
            Prelude.<*> (x Core..: "SecretArn")
            Prelude.<*> (x Core..: "OneDriveUsers")
      )

instance Prelude.Hashable OneDriveConfiguration where
  hashWithSalt _salt OneDriveConfiguration' {..} =
    _salt `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` disableLocalGroups
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` tenantDomain
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` oneDriveUsers

instance Prelude.NFData OneDriveConfiguration where
  rnf OneDriveConfiguration' {..} =
    Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf disableLocalGroups
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf tenantDomain
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf oneDriveUsers

instance Core.ToJSON OneDriveConfiguration where
  toJSON OneDriveConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("DisableLocalGroups" Core..=)
              Prelude.<$> disableLocalGroups,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            Prelude.Just ("TenantDomain" Core..= tenantDomain),
            Prelude.Just ("SecretArn" Core..= secretArn),
            Prelude.Just
              ("OneDriveUsers" Core..= oneDriveUsers)
          ]
      )
