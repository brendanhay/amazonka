{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Patch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Patch where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents metadata about a patch.
--
--
--
-- /See:/ 'patch' smart constructor.
data Patch = Patch'
  { _patBugzillaIds :: !(Maybe [Text]),
    _patVendor :: !(Maybe Text),
    _patMsrcSeverity :: !(Maybe Text),
    _patRepository :: !(Maybe Text),
    _patProductFamily :: !(Maybe Text),
    _patSeverity :: !(Maybe Text),
    _patAdvisoryIds :: !(Maybe [Text]),
    _patCVEIds :: !(Maybe [Text]),
    _patClassification :: !(Maybe Text),
    _patRelease :: !(Maybe Text),
    _patMsrcNumber :: !(Maybe Text),
    _patName :: !(Maybe Text),
    _patVersion :: !(Maybe Text),
    _patLanguage :: !(Maybe Text),
    _patKbNumber :: !(Maybe Text),
    _patContentURL :: !(Maybe Text),
    _patId :: !(Maybe Text),
    _patReleaseDate :: !(Maybe POSIX),
    _patTitle :: !(Maybe Text),
    _patArch :: !(Maybe Text),
    _patProduct :: !(Maybe Text),
    _patDescription :: !(Maybe Text),
    _patEpoch :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Patch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'patBugzillaIds' - The Bugzilla ID of the patch. For example, @1600646@ . Applies to Linux-based instances only.
--
-- * 'patVendor' - The name of the vendor providing the patch.
--
-- * 'patMsrcSeverity' - The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
--
-- * 'patRepository' - The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
--
-- * 'patProductFamily' - The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
--
-- * 'patSeverity' - The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
--
-- * 'patAdvisoryIds' - The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
--
-- * 'patCVEIds' - The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
--
-- * 'patClassification' - The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
--
-- * 'patRelease' - The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
--
-- * 'patMsrcNumber' - The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
--
-- * 'patName' - The name of the patch. Applies to Linux-based instances only.
--
-- * 'patVersion' - The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
--
-- * 'patLanguage' - The language of the patch if it's language-specific.
--
-- * 'patKbNumber' - The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
--
-- * 'patContentURL' - The URL where more information can be obtained about the patch.
--
-- * 'patId' - The ID of the patch. Applies to Windows patches only.
--
-- * 'patReleaseDate' - The date the patch was released.
--
-- * 'patTitle' - The title of the patch.
--
-- * 'patArch' - The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
--
-- * 'patProduct' - The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
--
-- * 'patDescription' - The description of the patch.
--
-- * 'patEpoch' - The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
patch ::
  Patch
patch =
  Patch'
    { _patBugzillaIds = Nothing,
      _patVendor = Nothing,
      _patMsrcSeverity = Nothing,
      _patRepository = Nothing,
      _patProductFamily = Nothing,
      _patSeverity = Nothing,
      _patAdvisoryIds = Nothing,
      _patCVEIds = Nothing,
      _patClassification = Nothing,
      _patRelease = Nothing,
      _patMsrcNumber = Nothing,
      _patName = Nothing,
      _patVersion = Nothing,
      _patLanguage = Nothing,
      _patKbNumber = Nothing,
      _patContentURL = Nothing,
      _patId = Nothing,
      _patReleaseDate = Nothing,
      _patTitle = Nothing,
      _patArch = Nothing,
      _patProduct = Nothing,
      _patDescription = Nothing,
      _patEpoch = Nothing
    }

-- | The Bugzilla ID of the patch. For example, @1600646@ . Applies to Linux-based instances only.
patBugzillaIds :: Lens' Patch [Text]
patBugzillaIds = lens _patBugzillaIds (\s a -> s {_patBugzillaIds = a}) . _Default . _Coerce

-- | The name of the vendor providing the patch.
patVendor :: Lens' Patch (Maybe Text)
patVendor = lens _patVendor (\s a -> s {_patVendor = a})

-- | The severity of the patch, such as @Critical@ , @Important@ , or @Moderate@ . Applies to Windows patches only.
patMsrcSeverity :: Lens' Patch (Maybe Text)
patMsrcSeverity = lens _patMsrcSeverity (\s a -> s {_patMsrcSeverity = a})

-- | The source patch repository for the operating system and version, such as @trusty-security@ for Ubuntu Server 14.04 LTE and @focal-security@ for Ubuntu Server 20.04 LTE. Applies to Linux-based instances only.
patRepository :: Lens' Patch (Maybe Text)
patRepository = lens _patRepository (\s a -> s {_patRepository = a})

-- | The product family the patch is applicable for. For example, @Windows@ or @Amazon Linux 2@ .
patProductFamily :: Lens' Patch (Maybe Text)
patProductFamily = lens _patProductFamily (\s a -> s {_patProductFamily = a})

-- | The severity level of the patch. For example, @CRITICAL@ or @MODERATE@ .
patSeverity :: Lens' Patch (Maybe Text)
patSeverity = lens _patSeverity (\s a -> s {_patSeverity = a})

-- | The Advisory ID of the patch. For example, @RHSA-2020:3779@ . Applies to Linux-based instances only.
patAdvisoryIds :: Lens' Patch [Text]
patAdvisoryIds = lens _patAdvisoryIds (\s a -> s {_patAdvisoryIds = a}) . _Default . _Coerce

-- | The Common Vulnerabilities and Exposures (CVE) ID of the patch. For example, @CVE-1999-0067@ . Applies to Linux-based instances only.
patCVEIds :: Lens' Patch [Text]
patCVEIds = lens _patCVEIds (\s a -> s {_patCVEIds = a}) . _Default . _Coerce

-- | The classification of the patch. For example, @SecurityUpdates@ , @Updates@ , or @CriticalUpdates@ .
patClassification :: Lens' Patch (Maybe Text)
patClassification = lens _patClassification (\s a -> s {_patClassification = a})

-- | The particular release of a patch. For example, in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the release is @2.amaz1@ . Applies to Linux-based instances only.
patRelease :: Lens' Patch (Maybe Text)
patRelease = lens _patRelease (\s a -> s {_patRelease = a})

-- | The ID of the Microsoft Security Response Center (MSRC) bulletin the patch is related to. For example, @MS14-045@ . Applies to Windows patches only.
patMsrcNumber :: Lens' Patch (Maybe Text)
patMsrcNumber = lens _patMsrcNumber (\s a -> s {_patMsrcNumber = a})

-- | The name of the patch. Applies to Linux-based instances only.
patName :: Lens' Patch (Maybe Text)
patName = lens _patName (\s a -> s {_patName = a})

-- | The version number of the patch. For example, in @example-pkg-1.710.10-2.7.abcd.x86_64@ , the version number is indicated by @-1@ . Applies to Linux-based instances only.
patVersion :: Lens' Patch (Maybe Text)
patVersion = lens _patVersion (\s a -> s {_patVersion = a})

-- | The language of the patch if it's language-specific.
patLanguage :: Lens' Patch (Maybe Text)
patLanguage = lens _patLanguage (\s a -> s {_patLanguage = a})

-- | The Microsoft Knowledge Base ID of the patch. Applies to Windows patches only.
patKbNumber :: Lens' Patch (Maybe Text)
patKbNumber = lens _patKbNumber (\s a -> s {_patKbNumber = a})

-- | The URL where more information can be obtained about the patch.
patContentURL :: Lens' Patch (Maybe Text)
patContentURL = lens _patContentURL (\s a -> s {_patContentURL = a})

-- | The ID of the patch. Applies to Windows patches only.
patId :: Lens' Patch (Maybe Text)
patId = lens _patId (\s a -> s {_patId = a})

-- | The date the patch was released.
patReleaseDate :: Lens' Patch (Maybe UTCTime)
patReleaseDate = lens _patReleaseDate (\s a -> s {_patReleaseDate = a}) . mapping _Time

-- | The title of the patch.
patTitle :: Lens' Patch (Maybe Text)
patTitle = lens _patTitle (\s a -> s {_patTitle = a})

-- | The architecture of the patch. For example, in @example-pkg-0.710.10-2.7.abcd.x86_64@ , the architecture is indicated by @x86_64@ . Applies to Linux-based instances only.
patArch :: Lens' Patch (Maybe Text)
patArch = lens _patArch (\s a -> s {_patArch = a})

-- | The specific product the patch is applicable for. For example, @WindowsServer2016@ or @AmazonLinux2018.03@ .
patProduct :: Lens' Patch (Maybe Text)
patProduct = lens _patProduct (\s a -> s {_patProduct = a})

-- | The description of the patch.
patDescription :: Lens' Patch (Maybe Text)
patDescription = lens _patDescription (\s a -> s {_patDescription = a})

-- | The epoch of the patch. For example in @pkg-example-EE-20180914-2.2.amzn1.noarch@ , the epoch value is @20180914-2@ . Applies to Linux-based instances only.
patEpoch :: Lens' Patch (Maybe Int)
patEpoch = lens _patEpoch (\s a -> s {_patEpoch = a})

instance FromJSON Patch where
  parseJSON =
    withObject
      "Patch"
      ( \x ->
          Patch'
            <$> (x .:? "BugzillaIds" .!= mempty)
            <*> (x .:? "Vendor")
            <*> (x .:? "MsrcSeverity")
            <*> (x .:? "Repository")
            <*> (x .:? "ProductFamily")
            <*> (x .:? "Severity")
            <*> (x .:? "AdvisoryIds" .!= mempty)
            <*> (x .:? "CVEIds" .!= mempty)
            <*> (x .:? "Classification")
            <*> (x .:? "Release")
            <*> (x .:? "MsrcNumber")
            <*> (x .:? "Name")
            <*> (x .:? "Version")
            <*> (x .:? "Language")
            <*> (x .:? "KbNumber")
            <*> (x .:? "ContentUrl")
            <*> (x .:? "Id")
            <*> (x .:? "ReleaseDate")
            <*> (x .:? "Title")
            <*> (x .:? "Arch")
            <*> (x .:? "Product")
            <*> (x .:? "Description")
            <*> (x .:? "Epoch")
      )

instance Hashable Patch

instance NFData Patch
