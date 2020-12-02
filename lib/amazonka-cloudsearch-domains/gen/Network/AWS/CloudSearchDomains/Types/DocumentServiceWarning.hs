{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A warning returned by the document service when an issue is discovered while processing an upload request.
--
--
--
-- /See:/ 'documentServiceWarning' smart constructor.
newtype DocumentServiceWarning = DocumentServiceWarning'
  { _dswMessage ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentServiceWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dswMessage' - The description for a warning returned by the document service.
documentServiceWarning ::
  DocumentServiceWarning
documentServiceWarning =
  DocumentServiceWarning' {_dswMessage = Nothing}

-- | The description for a warning returned by the document service.
dswMessage :: Lens' DocumentServiceWarning (Maybe Text)
dswMessage = lens _dswMessage (\s a -> s {_dswMessage = a})

instance FromJSON DocumentServiceWarning where
  parseJSON =
    withObject
      "DocumentServiceWarning"
      (\x -> DocumentServiceWarning' <$> (x .:? "message"))

instance Hashable DocumentServiceWarning

instance NFData DocumentServiceWarning
