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
-- Module      : Amazonka.SSM.Types.Patch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Patch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents metadata about a patch.
--
-- /See:/ 'newPatch' smart constructor.
data Patch = Patch'
  { -- | The product family the patch is applicable for. For example, @Windows@
    -- or @Amazon Linux 2@.
    productFamily :: Prelude.Maybe Prelude.Text,
    -- | The specific product the patch is applicable for. For example,
    -- @WindowsServer2016@ or @AmazonLinux2018.03@.
    product :: Prelude.Maybe Prelude.Text,
    -- | The severity level of the patch. For example, @CRITICAL@ or @MODERATE@.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The name of the patch. Applies to Linux-based managed nodes only.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Advisory ID of the patch. For example, @RHSA-2020:3779@. Applies to
    -- Linux-based managed nodes only.
    advisoryIds :: Prelude.Maybe [Prelude.Text],
    -- | The severity of the patch, such as @Critical@, @Important@, or
    -- @Moderate@. Applies to Windows patches only.
    msrcSeverity :: Prelude.Maybe Prelude.Text,
    -- | The source patch repository for the operating system and version, such
    -- as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@
    -- for Ubuntu Server 20.04 LTE. Applies to Linux-based managed nodes only.
    repository :: Prelude.Maybe Prelude.Text,
    -- | The date the patch was released.
    releaseDate :: Prelude.Maybe Core.POSIX,
    -- | The Microsoft Knowledge Base ID of the patch. Applies to Windows patches
    -- only.
    kbNumber :: Prelude.Maybe Prelude.Text,
    -- | The ID of the patch. Applies to Windows patches only.
    --
    -- This ID isn\'t the same as the Microsoft Knowledge Base ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the patch.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Bugzilla ID of the patch. For example, @1600646@. Applies to
    -- Linux-based managed nodes only.
    bugzillaIds :: Prelude.Maybe [Prelude.Text],
    -- | The title of the patch.
    title :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the patch. For example, in
    -- @example-pkg-0.710.10-2.7.abcd.x86_64@, the architecture is indicated by
    -- @x86_64@. Applies to Linux-based managed nodes only.
    arch :: Prelude.Maybe Prelude.Text,
    -- | The epoch of the patch. For example in
    -- @pkg-example-EE-20180914-2.2.amzn1.noarch@, the epoch value is
    -- @20180914-2@. Applies to Linux-based managed nodes only.
    epoch :: Prelude.Maybe Prelude.Int,
    -- | The Common Vulnerabilities and Exposures (CVE) ID of the patch. For
    -- example, @CVE-2011-3192@. Applies to Linux-based managed nodes only.
    cVEIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Microsoft Security Response Center (MSRC) bulletin the
    -- patch is related to. For example, @MS14-045@. Applies to Windows patches
    -- only.
    msrcNumber :: Prelude.Maybe Prelude.Text,
    -- | The particular release of a patch. For example, in
    -- @pkg-example-EE-20180914-2.2.amzn1.noarch@, the release is @2.amaz1@.
    -- Applies to Linux-based managed nodes only.
    release :: Prelude.Maybe Prelude.Text,
    -- | The language of the patch if it\'s language-specific.
    language :: Prelude.Maybe Prelude.Text,
    -- | The name of the vendor providing the patch.
    vendor :: Prelude.Maybe Prelude.Text,
    -- | The version number of the patch. For example, in
    -- @example-pkg-1.710.10-2.7.abcd.x86_64@, the version number is indicated
    -- by @-1@. Applies to Linux-based managed nodes only.
    version :: Prelude.Maybe Prelude.Text,
    -- | The URL where more information can be obtained about the patch.
    contentUrl :: Prelude.Maybe Prelude.Text,
    -- | The classification of the patch. For example, @SecurityUpdates@,
    -- @Updates@, or @CriticalUpdates@.
    classification :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Patch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productFamily', 'patch_productFamily' - The product family the patch is applicable for. For example, @Windows@
-- or @Amazon Linux 2@.
--
-- 'product', 'patch_product' - The specific product the patch is applicable for. For example,
-- @WindowsServer2016@ or @AmazonLinux2018.03@.
--
-- 'severity', 'patch_severity' - The severity level of the patch. For example, @CRITICAL@ or @MODERATE@.
--
-- 'name', 'patch_name' - The name of the patch. Applies to Linux-based managed nodes only.
--
-- 'advisoryIds', 'patch_advisoryIds' - The Advisory ID of the patch. For example, @RHSA-2020:3779@. Applies to
-- Linux-based managed nodes only.
--
-- 'msrcSeverity', 'patch_msrcSeverity' - The severity of the patch, such as @Critical@, @Important@, or
-- @Moderate@. Applies to Windows patches only.
--
-- 'repository', 'patch_repository' - The source patch repository for the operating system and version, such
-- as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@
-- for Ubuntu Server 20.04 LTE. Applies to Linux-based managed nodes only.
--
-- 'releaseDate', 'patch_releaseDate' - The date the patch was released.
--
-- 'kbNumber', 'patch_kbNumber' - The Microsoft Knowledge Base ID of the patch. Applies to Windows patches
-- only.
--
-- 'id', 'patch_id' - The ID of the patch. Applies to Windows patches only.
--
-- This ID isn\'t the same as the Microsoft Knowledge Base ID.
--
-- 'description', 'patch_description' - The description of the patch.
--
-- 'bugzillaIds', 'patch_bugzillaIds' - The Bugzilla ID of the patch. For example, @1600646@. Applies to
-- Linux-based managed nodes only.
--
-- 'title', 'patch_title' - The title of the patch.
--
-- 'arch', 'patch_arch' - The architecture of the patch. For example, in
-- @example-pkg-0.710.10-2.7.abcd.x86_64@, the architecture is indicated by
-- @x86_64@. Applies to Linux-based managed nodes only.
--
-- 'epoch', 'patch_epoch' - The epoch of the patch. For example in
-- @pkg-example-EE-20180914-2.2.amzn1.noarch@, the epoch value is
-- @20180914-2@. Applies to Linux-based managed nodes only.
--
-- 'cVEIds', 'patch_cVEIds' - The Common Vulnerabilities and Exposures (CVE) ID of the patch. For
-- example, @CVE-2011-3192@. Applies to Linux-based managed nodes only.
--
-- 'msrcNumber', 'patch_msrcNumber' - The ID of the Microsoft Security Response Center (MSRC) bulletin the
-- patch is related to. For example, @MS14-045@. Applies to Windows patches
-- only.
--
-- 'release', 'patch_release' - The particular release of a patch. For example, in
-- @pkg-example-EE-20180914-2.2.amzn1.noarch@, the release is @2.amaz1@.
-- Applies to Linux-based managed nodes only.
--
-- 'language', 'patch_language' - The language of the patch if it\'s language-specific.
--
-- 'vendor', 'patch_vendor' - The name of the vendor providing the patch.
--
-- 'version', 'patch_version' - The version number of the patch. For example, in
-- @example-pkg-1.710.10-2.7.abcd.x86_64@, the version number is indicated
-- by @-1@. Applies to Linux-based managed nodes only.
--
-- 'contentUrl', 'patch_contentUrl' - The URL where more information can be obtained about the patch.
--
-- 'classification', 'patch_classification' - The classification of the patch. For example, @SecurityUpdates@,
-- @Updates@, or @CriticalUpdates@.
newPatch ::
  Patch
newPatch =
  Patch'
    { productFamily = Prelude.Nothing,
      product = Prelude.Nothing,
      severity = Prelude.Nothing,
      name = Prelude.Nothing,
      advisoryIds = Prelude.Nothing,
      msrcSeverity = Prelude.Nothing,
      repository = Prelude.Nothing,
      releaseDate = Prelude.Nothing,
      kbNumber = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      bugzillaIds = Prelude.Nothing,
      title = Prelude.Nothing,
      arch = Prelude.Nothing,
      epoch = Prelude.Nothing,
      cVEIds = Prelude.Nothing,
      msrcNumber = Prelude.Nothing,
      release = Prelude.Nothing,
      language = Prelude.Nothing,
      vendor = Prelude.Nothing,
      version = Prelude.Nothing,
      contentUrl = Prelude.Nothing,
      classification = Prelude.Nothing
    }

-- | The product family the patch is applicable for. For example, @Windows@
-- or @Amazon Linux 2@.
patch_productFamily :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_productFamily = Lens.lens (\Patch' {productFamily} -> productFamily) (\s@Patch' {} a -> s {productFamily = a} :: Patch)

-- | The specific product the patch is applicable for. For example,
-- @WindowsServer2016@ or @AmazonLinux2018.03@.
patch_product :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_product = Lens.lens (\Patch' {product} -> product) (\s@Patch' {} a -> s {product = a} :: Patch)

-- | The severity level of the patch. For example, @CRITICAL@ or @MODERATE@.
patch_severity :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_severity = Lens.lens (\Patch' {severity} -> severity) (\s@Patch' {} a -> s {severity = a} :: Patch)

-- | The name of the patch. Applies to Linux-based managed nodes only.
patch_name :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_name = Lens.lens (\Patch' {name} -> name) (\s@Patch' {} a -> s {name = a} :: Patch)

-- | The Advisory ID of the patch. For example, @RHSA-2020:3779@. Applies to
-- Linux-based managed nodes only.
patch_advisoryIds :: Lens.Lens' Patch (Prelude.Maybe [Prelude.Text])
patch_advisoryIds = Lens.lens (\Patch' {advisoryIds} -> advisoryIds) (\s@Patch' {} a -> s {advisoryIds = a} :: Patch) Prelude.. Lens.mapping Lens.coerced

-- | The severity of the patch, such as @Critical@, @Important@, or
-- @Moderate@. Applies to Windows patches only.
patch_msrcSeverity :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_msrcSeverity = Lens.lens (\Patch' {msrcSeverity} -> msrcSeverity) (\s@Patch' {} a -> s {msrcSeverity = a} :: Patch)

-- | The source patch repository for the operating system and version, such
-- as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@
-- for Ubuntu Server 20.04 LTE. Applies to Linux-based managed nodes only.
patch_repository :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_repository = Lens.lens (\Patch' {repository} -> repository) (\s@Patch' {} a -> s {repository = a} :: Patch)

-- | The date the patch was released.
patch_releaseDate :: Lens.Lens' Patch (Prelude.Maybe Prelude.UTCTime)
patch_releaseDate = Lens.lens (\Patch' {releaseDate} -> releaseDate) (\s@Patch' {} a -> s {releaseDate = a} :: Patch) Prelude.. Lens.mapping Core._Time

-- | The Microsoft Knowledge Base ID of the patch. Applies to Windows patches
-- only.
patch_kbNumber :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_kbNumber = Lens.lens (\Patch' {kbNumber} -> kbNumber) (\s@Patch' {} a -> s {kbNumber = a} :: Patch)

-- | The ID of the patch. Applies to Windows patches only.
--
-- This ID isn\'t the same as the Microsoft Knowledge Base ID.
patch_id :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_id = Lens.lens (\Patch' {id} -> id) (\s@Patch' {} a -> s {id = a} :: Patch)

-- | The description of the patch.
patch_description :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_description = Lens.lens (\Patch' {description} -> description) (\s@Patch' {} a -> s {description = a} :: Patch)

-- | The Bugzilla ID of the patch. For example, @1600646@. Applies to
-- Linux-based managed nodes only.
patch_bugzillaIds :: Lens.Lens' Patch (Prelude.Maybe [Prelude.Text])
patch_bugzillaIds = Lens.lens (\Patch' {bugzillaIds} -> bugzillaIds) (\s@Patch' {} a -> s {bugzillaIds = a} :: Patch) Prelude.. Lens.mapping Lens.coerced

-- | The title of the patch.
patch_title :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_title = Lens.lens (\Patch' {title} -> title) (\s@Patch' {} a -> s {title = a} :: Patch)

-- | The architecture of the patch. For example, in
-- @example-pkg-0.710.10-2.7.abcd.x86_64@, the architecture is indicated by
-- @x86_64@. Applies to Linux-based managed nodes only.
patch_arch :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_arch = Lens.lens (\Patch' {arch} -> arch) (\s@Patch' {} a -> s {arch = a} :: Patch)

-- | The epoch of the patch. For example in
-- @pkg-example-EE-20180914-2.2.amzn1.noarch@, the epoch value is
-- @20180914-2@. Applies to Linux-based managed nodes only.
patch_epoch :: Lens.Lens' Patch (Prelude.Maybe Prelude.Int)
patch_epoch = Lens.lens (\Patch' {epoch} -> epoch) (\s@Patch' {} a -> s {epoch = a} :: Patch)

-- | The Common Vulnerabilities and Exposures (CVE) ID of the patch. For
-- example, @CVE-2011-3192@. Applies to Linux-based managed nodes only.
patch_cVEIds :: Lens.Lens' Patch (Prelude.Maybe [Prelude.Text])
patch_cVEIds = Lens.lens (\Patch' {cVEIds} -> cVEIds) (\s@Patch' {} a -> s {cVEIds = a} :: Patch) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Microsoft Security Response Center (MSRC) bulletin the
-- patch is related to. For example, @MS14-045@. Applies to Windows patches
-- only.
patch_msrcNumber :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_msrcNumber = Lens.lens (\Patch' {msrcNumber} -> msrcNumber) (\s@Patch' {} a -> s {msrcNumber = a} :: Patch)

-- | The particular release of a patch. For example, in
-- @pkg-example-EE-20180914-2.2.amzn1.noarch@, the release is @2.amaz1@.
-- Applies to Linux-based managed nodes only.
patch_release :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_release = Lens.lens (\Patch' {release} -> release) (\s@Patch' {} a -> s {release = a} :: Patch)

-- | The language of the patch if it\'s language-specific.
patch_language :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_language = Lens.lens (\Patch' {language} -> language) (\s@Patch' {} a -> s {language = a} :: Patch)

-- | The name of the vendor providing the patch.
patch_vendor :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_vendor = Lens.lens (\Patch' {vendor} -> vendor) (\s@Patch' {} a -> s {vendor = a} :: Patch)

-- | The version number of the patch. For example, in
-- @example-pkg-1.710.10-2.7.abcd.x86_64@, the version number is indicated
-- by @-1@. Applies to Linux-based managed nodes only.
patch_version :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_version = Lens.lens (\Patch' {version} -> version) (\s@Patch' {} a -> s {version = a} :: Patch)

-- | The URL where more information can be obtained about the patch.
patch_contentUrl :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_contentUrl = Lens.lens (\Patch' {contentUrl} -> contentUrl) (\s@Patch' {} a -> s {contentUrl = a} :: Patch)

-- | The classification of the patch. For example, @SecurityUpdates@,
-- @Updates@, or @CriticalUpdates@.
patch_classification :: Lens.Lens' Patch (Prelude.Maybe Prelude.Text)
patch_classification = Lens.lens (\Patch' {classification} -> classification) (\s@Patch' {} a -> s {classification = a} :: Patch)

instance Core.FromJSON Patch where
  parseJSON =
    Core.withObject
      "Patch"
      ( \x ->
          Patch'
            Prelude.<$> (x Core..:? "ProductFamily")
            Prelude.<*> (x Core..:? "Product")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "AdvisoryIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MsrcSeverity")
            Prelude.<*> (x Core..:? "Repository")
            Prelude.<*> (x Core..:? "ReleaseDate")
            Prelude.<*> (x Core..:? "KbNumber")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "BugzillaIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "Arch")
            Prelude.<*> (x Core..:? "Epoch")
            Prelude.<*> (x Core..:? "CVEIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MsrcNumber")
            Prelude.<*> (x Core..:? "Release")
            Prelude.<*> (x Core..:? "Language")
            Prelude.<*> (x Core..:? "Vendor")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "ContentUrl")
            Prelude.<*> (x Core..:? "Classification")
      )

instance Prelude.Hashable Patch where
  hashWithSalt _salt Patch' {..} =
    _salt `Prelude.hashWithSalt` productFamily
      `Prelude.hashWithSalt` product
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` advisoryIds
      `Prelude.hashWithSalt` msrcSeverity
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` releaseDate
      `Prelude.hashWithSalt` kbNumber
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` bugzillaIds
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` arch
      `Prelude.hashWithSalt` epoch
      `Prelude.hashWithSalt` cVEIds
      `Prelude.hashWithSalt` msrcNumber
      `Prelude.hashWithSalt` release
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` vendor
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` contentUrl
      `Prelude.hashWithSalt` classification

instance Prelude.NFData Patch where
  rnf Patch' {..} =
    Prelude.rnf productFamily
      `Prelude.seq` Prelude.rnf product
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf advisoryIds
      `Prelude.seq` Prelude.rnf msrcSeverity
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf releaseDate
      `Prelude.seq` Prelude.rnf kbNumber
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf bugzillaIds
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf arch
      `Prelude.seq` Prelude.rnf epoch
      `Prelude.seq` Prelude.rnf cVEIds
      `Prelude.seq` Prelude.rnf msrcNumber
      `Prelude.seq` Prelude.rnf release
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf vendor
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf contentUrl
      `Prelude.seq` Prelude.rnf
        classification
