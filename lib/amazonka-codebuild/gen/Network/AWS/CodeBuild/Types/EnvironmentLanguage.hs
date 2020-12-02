{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentLanguage where

import Network.AWS.CodeBuild.Types.EnvironmentImage
import Network.AWS.CodeBuild.Types.LanguageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of Docker images that are related by programming language and are managed by AWS CodeBuild.
--
--
--
-- /See:/ 'environmentLanguage' smart constructor.
data EnvironmentLanguage = EnvironmentLanguage'
  { _elImages ::
      !(Maybe [EnvironmentImage]),
    _elLanguage :: !(Maybe LanguageType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elImages' - The list of Docker images that are related by the specified programming language.
--
-- * 'elLanguage' - The programming language for the Docker images.
environmentLanguage ::
  EnvironmentLanguage
environmentLanguage =
  EnvironmentLanguage' {_elImages = Nothing, _elLanguage = Nothing}

-- | The list of Docker images that are related by the specified programming language.
elImages :: Lens' EnvironmentLanguage [EnvironmentImage]
elImages = lens _elImages (\s a -> s {_elImages = a}) . _Default . _Coerce

-- | The programming language for the Docker images.
elLanguage :: Lens' EnvironmentLanguage (Maybe LanguageType)
elLanguage = lens _elLanguage (\s a -> s {_elLanguage = a})

instance FromJSON EnvironmentLanguage where
  parseJSON =
    withObject
      "EnvironmentLanguage"
      ( \x ->
          EnvironmentLanguage'
            <$> (x .:? "images" .!= mempty) <*> (x .:? "language")
      )

instance Hashable EnvironmentLanguage

instance NFData EnvironmentLanguage
