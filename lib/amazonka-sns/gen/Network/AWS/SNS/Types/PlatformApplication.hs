{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.PlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.PlatformApplication where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Platform application object.
--
--
--
-- /See:/ 'platformApplication' smart constructor.
data PlatformApplication = PlatformApplication'
  { _paPlatformApplicationARN ::
      !(Maybe Text),
    _paAttributes :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlatformApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPlatformApplicationARN' - PlatformApplicationArn for platform application object.
--
-- * 'paAttributes' - Attributes for platform application object.
platformApplication ::
  PlatformApplication
platformApplication =
  PlatformApplication'
    { _paPlatformApplicationARN = Nothing,
      _paAttributes = Nothing
    }

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationARN :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationARN = lens _paPlatformApplicationARN (\s a -> s {_paPlatformApplicationARN = a})

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (HashMap Text (Text))
paAttributes = lens _paAttributes (\s a -> s {_paAttributes = a}) . _Default . _Map

instance FromXML PlatformApplication where
  parseXML x =
    PlatformApplication'
      <$> (x .@? "PlatformApplicationArn")
      <*> ( x .@? "Attributes" .!@ mempty
              >>= may (parseXMLMap "entry" "key" "value")
          )

instance Hashable PlatformApplication

instance NFData PlatformApplication
