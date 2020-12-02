{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A programming language supported by the platform.
--
--
--
-- /See:/ 'platformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { _pplName ::
      !(Maybe Text),
    _pplVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlatformProgrammingLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pplName' - The name of the programming language.
--
-- * 'pplVersion' - The version of the programming language.
platformProgrammingLanguage ::
  PlatformProgrammingLanguage
platformProgrammingLanguage =
  PlatformProgrammingLanguage'
    { _pplName = Nothing,
      _pplVersion = Nothing
    }

-- | The name of the programming language.
pplName :: Lens' PlatformProgrammingLanguage (Maybe Text)
pplName = lens _pplName (\s a -> s {_pplName = a})

-- | The version of the programming language.
pplVersion :: Lens' PlatformProgrammingLanguage (Maybe Text)
pplVersion = lens _pplVersion (\s a -> s {_pplVersion = a})

instance FromXML PlatformProgrammingLanguage where
  parseXML x =
    PlatformProgrammingLanguage'
      <$> (x .@? "Name") <*> (x .@? "Version")

instance Hashable PlatformProgrammingLanguage

instance NFData PlatformProgrammingLanguage
