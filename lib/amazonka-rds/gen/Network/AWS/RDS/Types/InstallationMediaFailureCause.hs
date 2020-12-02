{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.InstallationMediaFailureCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.InstallationMediaFailureCause where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the cause of an installation media failure. Installation media is used for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
--
--
-- /See:/ 'installationMediaFailureCause' smart constructor.
newtype InstallationMediaFailureCause = InstallationMediaFailureCause'
  { _imfcMessage ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstallationMediaFailureCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imfcMessage' - The reason that an installation media import failed.
installationMediaFailureCause ::
  InstallationMediaFailureCause
installationMediaFailureCause =
  InstallationMediaFailureCause' {_imfcMessage = Nothing}

-- | The reason that an installation media import failed.
imfcMessage :: Lens' InstallationMediaFailureCause (Maybe Text)
imfcMessage = lens _imfcMessage (\s a -> s {_imfcMessage = a})

instance FromXML InstallationMediaFailureCause where
  parseXML x = InstallationMediaFailureCause' <$> (x .@? "Message")

instance Hashable InstallationMediaFailureCause

instance NFData InstallationMediaFailureCause
