{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains directory limit information for a Region.
--
--
--
-- /See:/ 'directoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
  { _dlConnectedDirectoriesCurrentCount ::
      !(Maybe Nat),
    _dlCloudOnlyMicrosoftADLimitReached :: !(Maybe Bool),
    _dlConnectedDirectoriesLimit :: !(Maybe Nat),
    _dlConnectedDirectoriesLimitReached :: !(Maybe Bool),
    _dlCloudOnlyMicrosoftADLimit :: !(Maybe Nat),
    _dlCloudOnlyDirectoriesLimit :: !(Maybe Nat),
    _dlCloudOnlyDirectoriesCurrentCount :: !(Maybe Nat),
    _dlCloudOnlyDirectoriesLimitReached :: !(Maybe Bool),
    _dlCloudOnlyMicrosoftADCurrentCount :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlConnectedDirectoriesCurrentCount' - The current number of connected directories in the Region.
--
-- * 'dlCloudOnlyMicrosoftADLimitReached' - Indicates if the AWS Managed Microsoft AD directory limit has been reached.
--
-- * 'dlConnectedDirectoriesLimit' - The maximum number of connected directories allowed in the Region.
--
-- * 'dlConnectedDirectoriesLimitReached' - Indicates if the connected directory limit has been reached.
--
-- * 'dlCloudOnlyMicrosoftADLimit' - The maximum number of AWS Managed Microsoft AD directories allowed in the region.
--
-- * 'dlCloudOnlyDirectoriesLimit' - The maximum number of cloud directories allowed in the Region.
--
-- * 'dlCloudOnlyDirectoriesCurrentCount' - The current number of cloud directories in the Region.
--
-- * 'dlCloudOnlyDirectoriesLimitReached' - Indicates if the cloud directory limit has been reached.
--
-- * 'dlCloudOnlyMicrosoftADCurrentCount' - The current number of AWS Managed Microsoft AD directories in the region.
directoryLimits ::
  DirectoryLimits
directoryLimits =
  DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount = Nothing,
      _dlCloudOnlyMicrosoftADLimitReached = Nothing,
      _dlConnectedDirectoriesLimit = Nothing,
      _dlConnectedDirectoriesLimitReached = Nothing,
      _dlCloudOnlyMicrosoftADLimit = Nothing,
      _dlCloudOnlyDirectoriesLimit = Nothing,
      _dlCloudOnlyDirectoriesCurrentCount = Nothing,
      _dlCloudOnlyDirectoriesLimitReached = Nothing,
      _dlCloudOnlyMicrosoftADCurrentCount = Nothing
    }

-- | The current number of connected directories in the Region.
dlConnectedDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesCurrentCount = lens _dlConnectedDirectoriesCurrentCount (\s a -> s {_dlConnectedDirectoriesCurrentCount = a}) . mapping _Nat

-- | Indicates if the AWS Managed Microsoft AD directory limit has been reached.
dlCloudOnlyMicrosoftADLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyMicrosoftADLimitReached = lens _dlCloudOnlyMicrosoftADLimitReached (\s a -> s {_dlCloudOnlyMicrosoftADLimitReached = a})

-- | The maximum number of connected directories allowed in the Region.
dlConnectedDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesLimit = lens _dlConnectedDirectoriesLimit (\s a -> s {_dlConnectedDirectoriesLimit = a}) . mapping _Nat

-- | Indicates if the connected directory limit has been reached.
dlConnectedDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlConnectedDirectoriesLimitReached = lens _dlConnectedDirectoriesLimitReached (\s a -> s {_dlConnectedDirectoriesLimitReached = a})

-- | The maximum number of AWS Managed Microsoft AD directories allowed in the region.
dlCloudOnlyMicrosoftADLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyMicrosoftADLimit = lens _dlCloudOnlyMicrosoftADLimit (\s a -> s {_dlCloudOnlyMicrosoftADLimit = a}) . mapping _Nat

-- | The maximum number of cloud directories allowed in the Region.
dlCloudOnlyDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesLimit = lens _dlCloudOnlyDirectoriesLimit (\s a -> s {_dlCloudOnlyDirectoriesLimit = a}) . mapping _Nat

-- | The current number of cloud directories in the Region.
dlCloudOnlyDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesCurrentCount = lens _dlCloudOnlyDirectoriesCurrentCount (\s a -> s {_dlCloudOnlyDirectoriesCurrentCount = a}) . mapping _Nat

-- | Indicates if the cloud directory limit has been reached.
dlCloudOnlyDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyDirectoriesLimitReached = lens _dlCloudOnlyDirectoriesLimitReached (\s a -> s {_dlCloudOnlyDirectoriesLimitReached = a})

-- | The current number of AWS Managed Microsoft AD directories in the region.
dlCloudOnlyMicrosoftADCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyMicrosoftADCurrentCount = lens _dlCloudOnlyMicrosoftADCurrentCount (\s a -> s {_dlCloudOnlyMicrosoftADCurrentCount = a}) . mapping _Nat

instance FromJSON DirectoryLimits where
  parseJSON =
    withObject
      "DirectoryLimits"
      ( \x ->
          DirectoryLimits'
            <$> (x .:? "ConnectedDirectoriesCurrentCount")
            <*> (x .:? "CloudOnlyMicrosoftADLimitReached")
            <*> (x .:? "ConnectedDirectoriesLimit")
            <*> (x .:? "ConnectedDirectoriesLimitReached")
            <*> (x .:? "CloudOnlyMicrosoftADLimit")
            <*> (x .:? "CloudOnlyDirectoriesLimit")
            <*> (x .:? "CloudOnlyDirectoriesCurrentCount")
            <*> (x .:? "CloudOnlyDirectoriesLimitReached")
            <*> (x .:? "CloudOnlyMicrosoftADCurrentCount")
      )

instance Hashable DirectoryLimits

instance NFData DirectoryLimits
