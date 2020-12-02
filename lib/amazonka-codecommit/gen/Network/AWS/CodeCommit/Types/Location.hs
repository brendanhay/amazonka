{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Location where

import Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the location of a change or comment in the comparison between two commits or a pull request.
--
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lRelativeFileVersion ::
      !(Maybe RelativeFileVersionEnum),
    _lFilePath :: !(Maybe Text),
    _lFilePosition :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lRelativeFileVersion' - In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
--
-- * 'lFilePath' - The name of the file being compared, including its extension and subdirectory, if any.
--
-- * 'lFilePosition' - The position of a change in a compared file, in line number format.
location ::
  Location
location =
  Location'
    { _lRelativeFileVersion = Nothing,
      _lFilePath = Nothing,
      _lFilePosition = Nothing
    }

-- | In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
lRelativeFileVersion :: Lens' Location (Maybe RelativeFileVersionEnum)
lRelativeFileVersion = lens _lRelativeFileVersion (\s a -> s {_lRelativeFileVersion = a})

-- | The name of the file being compared, including its extension and subdirectory, if any.
lFilePath :: Lens' Location (Maybe Text)
lFilePath = lens _lFilePath (\s a -> s {_lFilePath = a})

-- | The position of a change in a compared file, in line number format.
lFilePosition :: Lens' Location (Maybe Integer)
lFilePosition = lens _lFilePosition (\s a -> s {_lFilePosition = a})

instance FromJSON Location where
  parseJSON =
    withObject
      "Location"
      ( \x ->
          Location'
            <$> (x .:? "relativeFileVersion")
            <*> (x .:? "filePath")
            <*> (x .:? "filePosition")
      )

instance Hashable Location

instance NFData Location

instance ToJSON Location where
  toJSON Location' {..} =
    object
      ( catMaybes
          [ ("relativeFileVersion" .=) <$> _lRelativeFileVersion,
            ("filePath" .=) <$> _lFilePath,
            ("filePosition" .=) <$> _lFilePosition
          ]
      )
