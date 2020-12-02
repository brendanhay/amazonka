{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentRequires
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentRequires where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An SSM document required by the current document.
--
--
--
-- /See:/ 'documentRequires' smart constructor.
data DocumentRequires = DocumentRequires'
  { _drVersion ::
      !(Maybe Text),
    _drName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentRequires' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drVersion' - The document version required by the current document.
--
-- * 'drName' - The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
documentRequires ::
  -- | 'drName'
  Text ->
  DocumentRequires
documentRequires pName_ =
  DocumentRequires' {_drVersion = Nothing, _drName = pName_}

-- | The document version required by the current document.
drVersion :: Lens' DocumentRequires (Maybe Text)
drVersion = lens _drVersion (\s a -> s {_drVersion = a})

-- | The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
drName :: Lens' DocumentRequires Text
drName = lens _drName (\s a -> s {_drName = a})

instance FromJSON DocumentRequires where
  parseJSON =
    withObject
      "DocumentRequires"
      (\x -> DocumentRequires' <$> (x .:? "Version") <*> (x .: "Name"))

instance Hashable DocumentRequires

instance NFData DocumentRequires

instance ToJSON DocumentRequires where
  toJSON DocumentRequires' {..} =
    object
      ( catMaybes
          [("Version" .=) <$> _drVersion, Just ("Name" .= _drName)]
      )
