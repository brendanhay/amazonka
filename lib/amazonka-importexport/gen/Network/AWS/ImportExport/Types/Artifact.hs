{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Artifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types.Artifact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A discrete item that contains the description and URL of an artifact (such as a PDF).
--
-- /See:/ 'artifact' smart constructor.
data Artifact = Artifact'
  { _aURL :: !(Maybe Text),
    _aDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Artifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aURL' - Undocumented member.
--
-- * 'aDescription' - Undocumented member.
artifact ::
  Artifact
artifact = Artifact' {_aURL = Nothing, _aDescription = Nothing}

-- | Undocumented member.
aURL :: Lens' Artifact (Maybe Text)
aURL = lens _aURL (\s a -> s {_aURL = a})

-- | Undocumented member.
aDescription :: Lens' Artifact (Maybe Text)
aDescription = lens _aDescription (\s a -> s {_aDescription = a})

instance FromXML Artifact where
  parseXML x = Artifact' <$> (x .@? "URL") <*> (x .@? "Description")

instance Hashable Artifact

instance NFData Artifact
