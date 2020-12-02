{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFinding where

import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.FindingSeverity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an image scan finding.
--
--
--
-- /See:/ 'imageScanFinding' smart constructor.
data ImageScanFinding = ImageScanFinding'
  { _isfSeverity ::
      !(Maybe FindingSeverity),
    _isfUri :: !(Maybe Text),
    _isfName :: !(Maybe Text),
    _isfAttributes :: !(Maybe [Attribute]),
    _isfDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageScanFinding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isfSeverity' - The finding severity.
--
-- * 'isfUri' - A link containing additional details about the security vulnerability.
--
-- * 'isfName' - The name associated with the finding, usually a CVE number.
--
-- * 'isfAttributes' - A collection of attributes of the host from which the finding is generated.
--
-- * 'isfDescription' - The description of the finding.
imageScanFinding ::
  ImageScanFinding
imageScanFinding =
  ImageScanFinding'
    { _isfSeverity = Nothing,
      _isfUri = Nothing,
      _isfName = Nothing,
      _isfAttributes = Nothing,
      _isfDescription = Nothing
    }

-- | The finding severity.
isfSeverity :: Lens' ImageScanFinding (Maybe FindingSeverity)
isfSeverity = lens _isfSeverity (\s a -> s {_isfSeverity = a})

-- | A link containing additional details about the security vulnerability.
isfUri :: Lens' ImageScanFinding (Maybe Text)
isfUri = lens _isfUri (\s a -> s {_isfUri = a})

-- | The name associated with the finding, usually a CVE number.
isfName :: Lens' ImageScanFinding (Maybe Text)
isfName = lens _isfName (\s a -> s {_isfName = a})

-- | A collection of attributes of the host from which the finding is generated.
isfAttributes :: Lens' ImageScanFinding [Attribute]
isfAttributes = lens _isfAttributes (\s a -> s {_isfAttributes = a}) . _Default . _Coerce

-- | The description of the finding.
isfDescription :: Lens' ImageScanFinding (Maybe Text)
isfDescription = lens _isfDescription (\s a -> s {_isfDescription = a})

instance FromJSON ImageScanFinding where
  parseJSON =
    withObject
      "ImageScanFinding"
      ( \x ->
          ImageScanFinding'
            <$> (x .:? "severity")
            <*> (x .:? "uri")
            <*> (x .:? "name")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "description")
      )

instance Hashable ImageScanFinding

instance NFData ImageScanFinding
