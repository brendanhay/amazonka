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
-- Module      : Network.AWS.Kendra.Types.OneDriveConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.OneDriveConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import Network.AWS.Kendra.Types.OneDriveUsers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for data sources that connect to
-- OneDrive.
--
-- /See:/ 'newOneDriveConfiguration' smart constructor.
data OneDriveConfiguration = OneDriveConfiguration'
  { -- | A list of @DataSourceToIndexFieldMapping@ objects that map Microsoft
    -- OneDrive fields to custom fields in the Amazon Kendra index. You must
    -- first create the index fields before you map OneDrive fields.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | List of regular expressions applied to documents. Items that match the
    -- exclusion pattern are not indexed. If you provide both an inclusion
    -- pattern and an exclusion pattern, any item that matches the exclusion
    -- pattern isn\'t indexed.
    --
    -- The exclusion pattern is applied to the file name.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A Boolean value that specifies whether local groups are disabled
    -- (@True@) or enabled (@False@).
    disableLocalGroups :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns. Documents that match the pattern
    -- are included in the index. Documents that don\'t match the pattern are
    -- excluded from the index. If a document matches both an inclusion pattern
    -- and an exclusion pattern, the document is not included in the index.
    --
    -- The exclusion pattern is applied to the file name.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
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
-- 'fieldMappings', 'oneDriveConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map Microsoft
-- OneDrive fields to custom fields in the Amazon Kendra index. You must
-- first create the index fields before you map OneDrive fields.
--
-- 'exclusionPatterns', 'oneDriveConfiguration_exclusionPatterns' - List of regular expressions applied to documents. Items that match the
-- exclusion pattern are not indexed. If you provide both an inclusion
-- pattern and an exclusion pattern, any item that matches the exclusion
-- pattern isn\'t indexed.
--
-- The exclusion pattern is applied to the file name.
--
-- 'disableLocalGroups', 'oneDriveConfiguration_disableLocalGroups' - A Boolean value that specifies whether local groups are disabled
-- (@True@) or enabled (@False@).
--
-- 'inclusionPatterns', 'oneDriveConfiguration_inclusionPatterns' - A list of regular expression patterns. Documents that match the pattern
-- are included in the index. Documents that don\'t match the pattern are
-- excluded from the index. If a document matches both an inclusion pattern
-- and an exclusion pattern, the document is not included in the index.
--
-- The exclusion pattern is applied to the file name.
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
      { fieldMappings =
          Prelude.Nothing,
        exclusionPatterns = Prelude.Nothing,
        disableLocalGroups = Prelude.Nothing,
        inclusionPatterns = Prelude.Nothing,
        tenantDomain = pTenantDomain_,
        secretArn = pSecretArn_,
        oneDriveUsers = pOneDriveUsers_
      }

-- | A list of @DataSourceToIndexFieldMapping@ objects that map Microsoft
-- OneDrive fields to custom fields in the Amazon Kendra index. You must
-- first create the index fields before you map OneDrive fields.
oneDriveConfiguration_fieldMappings :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
oneDriveConfiguration_fieldMappings = Lens.lens (\OneDriveConfiguration' {fieldMappings} -> fieldMappings) (\s@OneDriveConfiguration' {} a -> s {fieldMappings = a} :: OneDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | List of regular expressions applied to documents. Items that match the
-- exclusion pattern are not indexed. If you provide both an inclusion
-- pattern and an exclusion pattern, any item that matches the exclusion
-- pattern isn\'t indexed.
--
-- The exclusion pattern is applied to the file name.
oneDriveConfiguration_exclusionPatterns :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe [Prelude.Text])
oneDriveConfiguration_exclusionPatterns = Lens.lens (\OneDriveConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@OneDriveConfiguration' {} a -> s {exclusionPatterns = a} :: OneDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that specifies whether local groups are disabled
-- (@True@) or enabled (@False@).
oneDriveConfiguration_disableLocalGroups :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe Prelude.Bool)
oneDriveConfiguration_disableLocalGroups = Lens.lens (\OneDriveConfiguration' {disableLocalGroups} -> disableLocalGroups) (\s@OneDriveConfiguration' {} a -> s {disableLocalGroups = a} :: OneDriveConfiguration)

-- | A list of regular expression patterns. Documents that match the pattern
-- are included in the index. Documents that don\'t match the pattern are
-- excluded from the index. If a document matches both an inclusion pattern
-- and an exclusion pattern, the document is not included in the index.
--
-- The exclusion pattern is applied to the file name.
oneDriveConfiguration_inclusionPatterns :: Lens.Lens' OneDriveConfiguration (Prelude.Maybe [Prelude.Text])
oneDriveConfiguration_inclusionPatterns = Lens.lens (\OneDriveConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@OneDriveConfiguration' {} a -> s {inclusionPatterns = a} :: OneDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DisableLocalGroups")
            Prelude.<*> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "TenantDomain")
            Prelude.<*> (x Core..: "SecretArn")
            Prelude.<*> (x Core..: "OneDriveUsers")
      )

instance Prelude.Hashable OneDriveConfiguration

instance Prelude.NFData OneDriveConfiguration

instance Core.ToJSON OneDriveConfiguration where
  toJSON OneDriveConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            ("DisableLocalGroups" Core..=)
              Prelude.<$> disableLocalGroups,
            ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            Prelude.Just ("TenantDomain" Core..= tenantDomain),
            Prelude.Just ("SecretArn" Core..= secretArn),
            Prelude.Just
              ("OneDriveUsers" Core..= oneDriveUsers)
          ]
      )
