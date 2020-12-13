{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Patch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Patch
  ( Patch (..),

    -- * Smart constructor
    mkPatch,

    -- * Lenses
    pfBugzillaIds,
    pfVendor,
    pfMsrcSeverity,
    pfRepository,
    pfProductFamily,
    pfSeverity,
    pfAdvisoryIds,
    pfCVEIds,
    pfClassification,
    pfRelease,
    pfMsrcNumber,
    pfName,
    pfVersion,
    pfLanguage,
    pfKbNumber,
    pfContentURL,
    pfId,
    pfReleaseDate,
    pfTitle,
    pfArch,
    pfProduct,
    pfDescription,
    pfEpoch,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents metadata about a patch.
--
-- /See:/ 'mkPatch' smart constructor.
data Patch = Patch'
  { -- | The Bugzilla ID of the patch. For example, @1600646@ . Applies to Linux-based instances only.
    bugzillaIds :: Lude.Maybe [Lude.Text],
    -- | The name of the vendor providing the patch.
    vendor :: Lude.Maybe Lude.Text,
    -- | The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
    msrcSeverity :: Lude.Maybe Lude.Text,
    -- | The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
    repository :: Lude.Maybe Lude.Text,
    -- | The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
    productFamily :: Lude.Maybe Lude.Text,
    -- | The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
    severity :: Lude.Maybe Lude.Text,
    -- | The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
    advisoryIds :: Lude.Maybe [Lude.Text],
    -- | The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
    cVEIds :: Lude.Maybe [Lude.Text],
    -- | The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
    classification :: Lude.Maybe Lude.Text,
    -- | The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
    release :: Lude.Maybe Lude.Text,
    -- | The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
    msrcNumber :: Lude.Maybe Lude.Text,
    -- | The name of the patch. Applies to Linux-based instances only.
    name :: Lude.Maybe Lude.Text,
    -- | The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
    version :: Lude.Maybe Lude.Text,
    -- | The language of the patch if it's language-specific.
    language :: Lude.Maybe Lude.Text,
    -- | The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
    kbNumber :: Lude.Maybe Lude.Text,
    -- | The URL where more information can be obtained about the patch.
    contentURL :: Lude.Maybe Lude.Text,
    -- | The ID of the patch. Applies to Windows patches only.
    id :: Lude.Maybe Lude.Text,
    -- | The date the patch was released.
    releaseDate :: Lude.Maybe Lude.Timestamp,
    -- | The title of the patch.
    title :: Lude.Maybe Lude.Text,
    -- | The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
    arch :: Lude.Maybe Lude.Text,
    -- | The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
    product :: Lude.Maybe Lude.Text,
    -- | The description of the patch.
    description :: Lude.Maybe Lude.Text,
    -- | The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
    epoch :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Patch' with the minimum fields required to make a request.
--
-- * 'bugzillaIds' - The Bugzilla ID of the patch. For example, @1600646@ . Applies to Linux-based instances only.
-- * 'vendor' - The name of the vendor providing the patch.
-- * 'msrcSeverity' - The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
-- * 'repository' - The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
-- * 'productFamily' - The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
-- * 'severity' - The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
-- * 'advisoryIds' - The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
-- * 'cVEIds' - The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
-- * 'classification' - The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
-- * 'release' - The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
-- * 'msrcNumber' - The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
-- * 'name' - The name of the patch. Applies to Linux-based instances only.
-- * 'version' - The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
-- * 'language' - The language of the patch if it's language-specific.
-- * 'kbNumber' - The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
-- * 'contentURL' - The URL where more information can be obtained about the patch.
-- * 'id' - The ID of the patch. Applies to Windows patches only.
-- * 'releaseDate' - The date the patch was released.
-- * 'title' - The title of the patch.
-- * 'arch' - The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
-- * 'product' - The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
-- * 'description' - The description of the patch.
-- * 'epoch' - The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
mkPatch ::
  Patch
mkPatch =
  Patch'
    { bugzillaIds = Lude.Nothing,
      vendor = Lude.Nothing,
      msrcSeverity = Lude.Nothing,
      repository = Lude.Nothing,
      productFamily = Lude.Nothing,
      severity = Lude.Nothing,
      advisoryIds = Lude.Nothing,
      cVEIds = Lude.Nothing,
      classification = Lude.Nothing,
      release = Lude.Nothing,
      msrcNumber = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      language = Lude.Nothing,
      kbNumber = Lude.Nothing,
      contentURL = Lude.Nothing,
      id = Lude.Nothing,
      releaseDate = Lude.Nothing,
      title = Lude.Nothing,
      arch = Lude.Nothing,
      product = Lude.Nothing,
      description = Lude.Nothing,
      epoch = Lude.Nothing
    }

-- | The Bugzilla ID of the patch. For example, @1600646@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'bugzillaIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfBugzillaIds :: Lens.Lens' Patch (Lude.Maybe [Lude.Text])
pfBugzillaIds = Lens.lens (bugzillaIds :: Patch -> Lude.Maybe [Lude.Text]) (\s a -> s {bugzillaIds = a} :: Patch)
{-# DEPRECATED pfBugzillaIds "Use generic-lens or generic-optics with 'bugzillaIds' instead." #-}

-- | The name of the vendor providing the patch.
--
-- /Note:/ Consider using 'vendor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfVendor :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfVendor = Lens.lens (vendor :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {vendor = a} :: Patch)
{-# DEPRECATED pfVendor "Use generic-lens or generic-optics with 'vendor' instead." #-}

-- | The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
--
-- /Note:/ Consider using 'msrcSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfMsrcSeverity :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfMsrcSeverity = Lens.lens (msrcSeverity :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {msrcSeverity = a} :: Patch)
{-# DEPRECATED pfMsrcSeverity "Use generic-lens or generic-optics with 'msrcSeverity' instead." #-}

-- | The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfRepository :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfRepository = Lens.lens (repository :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {repository = a} :: Patch)
{-# DEPRECATED pfRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

-- | The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
--
-- /Note:/ Consider using 'productFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfProductFamily :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfProductFamily = Lens.lens (productFamily :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {productFamily = a} :: Patch)
{-# DEPRECATED pfProductFamily "Use generic-lens or generic-optics with 'productFamily' instead." #-}

-- | The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfSeverity :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfSeverity = Lens.lens (severity :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: Patch)
{-# DEPRECATED pfSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'advisoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfAdvisoryIds :: Lens.Lens' Patch (Lude.Maybe [Lude.Text])
pfAdvisoryIds = Lens.lens (advisoryIds :: Patch -> Lude.Maybe [Lude.Text]) (\s a -> s {advisoryIds = a} :: Patch)
{-# DEPRECATED pfAdvisoryIds "Use generic-lens or generic-optics with 'advisoryIds' instead." #-}

-- | The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'cVEIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfCVEIds :: Lens.Lens' Patch (Lude.Maybe [Lude.Text])
pfCVEIds = Lens.lens (cVEIds :: Patch -> Lude.Maybe [Lude.Text]) (\s a -> s {cVEIds = a} :: Patch)
{-# DEPRECATED pfCVEIds "Use generic-lens or generic-optics with 'cVEIds' instead." #-}

-- | The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfClassification :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfClassification = Lens.lens (classification :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {classification = a} :: Patch)
{-# DEPRECATED pfClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'release' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfRelease :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfRelease = Lens.lens (release :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {release = a} :: Patch)
{-# DEPRECATED pfRelease "Use generic-lens or generic-optics with 'release' instead." #-}

-- | The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
--
-- /Note:/ Consider using 'msrcNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfMsrcNumber :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfMsrcNumber = Lens.lens (msrcNumber :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {msrcNumber = a} :: Patch)
{-# DEPRECATED pfMsrcNumber "Use generic-lens or generic-optics with 'msrcNumber' instead." #-}

-- | The name of the patch. Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfName = Lens.lens (name :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Patch)
{-# DEPRECATED pfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfVersion :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfVersion = Lens.lens (version :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Patch)
{-# DEPRECATED pfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The language of the patch if it's language-specific.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfLanguage :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfLanguage = Lens.lens (language :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: Patch)
{-# DEPRECATED pfLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
--
-- /Note:/ Consider using 'kbNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfKbNumber :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfKbNumber = Lens.lens (kbNumber :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {kbNumber = a} :: Patch)
{-# DEPRECATED pfKbNumber "Use generic-lens or generic-optics with 'kbNumber' instead." #-}

-- | The URL where more information can be obtained about the patch.
--
-- /Note:/ Consider using 'contentURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfContentURL :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfContentURL = Lens.lens (contentURL :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {contentURL = a} :: Patch)
{-# DEPRECATED pfContentURL "Use generic-lens or generic-optics with 'contentURL' instead." #-}

-- | The ID of the patch. Applies to Windows patches only.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfId :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfId = Lens.lens (id :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Patch)
{-# DEPRECATED pfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date the patch was released.
--
-- /Note:/ Consider using 'releaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfReleaseDate :: Lens.Lens' Patch (Lude.Maybe Lude.Timestamp)
pfReleaseDate = Lens.lens (releaseDate :: Patch -> Lude.Maybe Lude.Timestamp) (\s a -> s {releaseDate = a} :: Patch)
{-# DEPRECATED pfReleaseDate "Use generic-lens or generic-optics with 'releaseDate' instead." #-}

-- | The title of the patch.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfTitle :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfTitle = Lens.lens (title :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: Patch)
{-# DEPRECATED pfTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'arch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfArch :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfArch = Lens.lens (arch :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {arch = a} :: Patch)
{-# DEPRECATED pfArch "Use generic-lens or generic-optics with 'arch' instead." #-}

-- | The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
--
-- /Note:/ Consider using 'product' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfProduct :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfProduct = Lens.lens (product :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {product = a} :: Patch)
{-# DEPRECATED pfProduct "Use generic-lens or generic-optics with 'product' instead." #-}

-- | The description of the patch.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfDescription :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
pfDescription = Lens.lens (description :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Patch)
{-# DEPRECATED pfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'epoch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfEpoch :: Lens.Lens' Patch (Lude.Maybe Lude.Int)
pfEpoch = Lens.lens (epoch :: Patch -> Lude.Maybe Lude.Int) (\s a -> s {epoch = a} :: Patch)
{-# DEPRECATED pfEpoch "Use generic-lens or generic-optics with 'epoch' instead." #-}

instance Lude.FromJSON Patch where
  parseJSON =
    Lude.withObject
      "Patch"
      ( \x ->
          Patch'
            Lude.<$> (x Lude..:? "BugzillaIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Vendor")
            Lude.<*> (x Lude..:? "MsrcSeverity")
            Lude.<*> (x Lude..:? "Repository")
            Lude.<*> (x Lude..:? "ProductFamily")
            Lude.<*> (x Lude..:? "Severity")
            Lude.<*> (x Lude..:? "AdvisoryIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CVEIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Classification")
            Lude.<*> (x Lude..:? "Release")
            Lude.<*> (x Lude..:? "MsrcNumber")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Language")
            Lude.<*> (x Lude..:? "KbNumber")
            Lude.<*> (x Lude..:? "ContentUrl")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "ReleaseDate")
            Lude.<*> (x Lude..:? "Title")
            Lude.<*> (x Lude..:? "Arch")
            Lude.<*> (x Lude..:? "Product")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Epoch")
      )
