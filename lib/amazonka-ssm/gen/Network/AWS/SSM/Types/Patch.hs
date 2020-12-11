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
    patBugzillaIds,
    patVendor,
    patMsrcSeverity,
    patRepository,
    patProductFamily,
    patSeverity,
    patAdvisoryIds,
    patCVEIds,
    patClassification,
    patRelease,
    patMsrcNumber,
    patName,
    patVersion,
    patLanguage,
    patKbNumber,
    patContentURL,
    patId,
    patReleaseDate,
    patTitle,
    patArch,
    patProduct,
    patDescription,
    patEpoch,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents metadata about a patch.
--
-- /See:/ 'mkPatch' smart constructor.
data Patch = Patch'
  { bugzillaIds :: Lude.Maybe [Lude.Text],
    vendor :: Lude.Maybe Lude.Text,
    msrcSeverity :: Lude.Maybe Lude.Text,
    repository :: Lude.Maybe Lude.Text,
    productFamily :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe Lude.Text,
    advisoryIds :: Lude.Maybe [Lude.Text],
    cVEIds :: Lude.Maybe [Lude.Text],
    classification :: Lude.Maybe Lude.Text,
    release :: Lude.Maybe Lude.Text,
    msrcNumber :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    language :: Lude.Maybe Lude.Text,
    kbNumber :: Lude.Maybe Lude.Text,
    contentURL :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    releaseDate :: Lude.Maybe Lude.Timestamp,
    title :: Lude.Maybe Lude.Text,
    arch :: Lude.Maybe Lude.Text,
    product :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    epoch :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Patch' with the minimum fields required to make a request.
--
-- * 'advisoryIds' - The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
-- * 'arch' - The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
-- * 'bugzillaIds' - The Bugzilla ID of the patch. For example, @1600646@ . Applies to Linux-based instances only.
-- * 'cVEIds' - The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
-- * 'classification' - The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
-- * 'contentURL' - The URL where more information can be obtained about the patch.
-- * 'description' - The description of the patch.
-- * 'epoch' - The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
-- * 'id' - The ID of the patch. Applies to Windows patches only.
-- * 'kbNumber' - The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
-- * 'language' - The language of the patch if it's language-specific.
-- * 'msrcNumber' - The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
-- * 'msrcSeverity' - The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
-- * 'name' - The name of the patch. Applies to Linux-based instances only.
-- * 'product' - The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
-- * 'productFamily' - The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
-- * 'release' - The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
-- * 'releaseDate' - The date the patch was released.
-- * 'repository' - The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
-- * 'severity' - The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
-- * 'title' - The title of the patch.
-- * 'vendor' - The name of the vendor providing the patch.
-- * 'version' - The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
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
patBugzillaIds :: Lens.Lens' Patch (Lude.Maybe [Lude.Text])
patBugzillaIds = Lens.lens (bugzillaIds :: Patch -> Lude.Maybe [Lude.Text]) (\s a -> s {bugzillaIds = a} :: Patch)
{-# DEPRECATED patBugzillaIds "Use generic-lens or generic-optics with 'bugzillaIds' instead." #-}

-- | The name of the vendor providing the patch.
--
-- /Note:/ Consider using 'vendor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patVendor :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patVendor = Lens.lens (vendor :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {vendor = a} :: Patch)
{-# DEPRECATED patVendor "Use generic-lens or generic-optics with 'vendor' instead." #-}

-- | The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
--
-- /Note:/ Consider using 'msrcSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patMsrcSeverity :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patMsrcSeverity = Lens.lens (msrcSeverity :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {msrcSeverity = a} :: Patch)
{-# DEPRECATED patMsrcSeverity "Use generic-lens or generic-optics with 'msrcSeverity' instead." #-}

-- | The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'repository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patRepository :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patRepository = Lens.lens (repository :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {repository = a} :: Patch)
{-# DEPRECATED patRepository "Use generic-lens or generic-optics with 'repository' instead." #-}

-- | The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
--
-- /Note:/ Consider using 'productFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patProductFamily :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patProductFamily = Lens.lens (productFamily :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {productFamily = a} :: Patch)
{-# DEPRECATED patProductFamily "Use generic-lens or generic-optics with 'productFamily' instead." #-}

-- | The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patSeverity :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patSeverity = Lens.lens (severity :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: Patch)
{-# DEPRECATED patSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'advisoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patAdvisoryIds :: Lens.Lens' Patch (Lude.Maybe [Lude.Text])
patAdvisoryIds = Lens.lens (advisoryIds :: Patch -> Lude.Maybe [Lude.Text]) (\s a -> s {advisoryIds = a} :: Patch)
{-# DEPRECATED patAdvisoryIds "Use generic-lens or generic-optics with 'advisoryIds' instead." #-}

-- | The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'cVEIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patCVEIds :: Lens.Lens' Patch (Lude.Maybe [Lude.Text])
patCVEIds = Lens.lens (cVEIds :: Patch -> Lude.Maybe [Lude.Text]) (\s a -> s {cVEIds = a} :: Patch)
{-# DEPRECATED patCVEIds "Use generic-lens or generic-optics with 'cVEIds' instead." #-}

-- | The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patClassification :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patClassification = Lens.lens (classification :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {classification = a} :: Patch)
{-# DEPRECATED patClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'release' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patRelease :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patRelease = Lens.lens (release :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {release = a} :: Patch)
{-# DEPRECATED patRelease "Use generic-lens or generic-optics with 'release' instead." #-}

-- | The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
--
-- /Note:/ Consider using 'msrcNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patMsrcNumber :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patMsrcNumber = Lens.lens (msrcNumber :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {msrcNumber = a} :: Patch)
{-# DEPRECATED patMsrcNumber "Use generic-lens or generic-optics with 'msrcNumber' instead." #-}

-- | The name of the patch. Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patName :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patName = Lens.lens (name :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Patch)
{-# DEPRECATED patName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patVersion :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patVersion = Lens.lens (version :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Patch)
{-# DEPRECATED patVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The language of the patch if it's language-specific.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patLanguage :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patLanguage = Lens.lens (language :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: Patch)
{-# DEPRECATED patLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
--
-- /Note:/ Consider using 'kbNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patKbNumber :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patKbNumber = Lens.lens (kbNumber :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {kbNumber = a} :: Patch)
{-# DEPRECATED patKbNumber "Use generic-lens or generic-optics with 'kbNumber' instead." #-}

-- | The URL where more information can be obtained about the patch.
--
-- /Note:/ Consider using 'contentURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patContentURL :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patContentURL = Lens.lens (contentURL :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {contentURL = a} :: Patch)
{-# DEPRECATED patContentURL "Use generic-lens or generic-optics with 'contentURL' instead." #-}

-- | The ID of the patch. Applies to Windows patches only.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patId :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patId = Lens.lens (id :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Patch)
{-# DEPRECATED patId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date the patch was released.
--
-- /Note:/ Consider using 'releaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patReleaseDate :: Lens.Lens' Patch (Lude.Maybe Lude.Timestamp)
patReleaseDate = Lens.lens (releaseDate :: Patch -> Lude.Maybe Lude.Timestamp) (\s a -> s {releaseDate = a} :: Patch)
{-# DEPRECATED patReleaseDate "Use generic-lens or generic-optics with 'releaseDate' instead." #-}

-- | The title of the patch.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patTitle :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patTitle = Lens.lens (title :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: Patch)
{-# DEPRECATED patTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'arch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patArch :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patArch = Lens.lens (arch :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {arch = a} :: Patch)
{-# DEPRECATED patArch "Use generic-lens or generic-optics with 'arch' instead." #-}

-- | The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
--
-- /Note:/ Consider using 'product' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patProduct :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patProduct = Lens.lens (product :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {product = a} :: Patch)
{-# DEPRECATED patProduct "Use generic-lens or generic-optics with 'product' instead." #-}

-- | The description of the patch.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patDescription :: Lens.Lens' Patch (Lude.Maybe Lude.Text)
patDescription = Lens.lens (description :: Patch -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Patch)
{-# DEPRECATED patDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
--
-- /Note:/ Consider using 'epoch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patEpoch :: Lens.Lens' Patch (Lude.Maybe Lude.Int)
patEpoch = Lens.lens (epoch :: Patch -> Lude.Maybe Lude.Int) (\s a -> s {epoch = a} :: Patch)
{-# DEPRECATED patEpoch "Use generic-lens or generic-optics with 'epoch' instead." #-}

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
