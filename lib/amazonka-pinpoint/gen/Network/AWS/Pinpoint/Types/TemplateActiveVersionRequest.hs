{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies which version of a message template to use as the active version of the template.
--
--
--
-- /See:/ 'templateActiveVersionRequest' smart constructor.
newtype TemplateActiveVersionRequest = TemplateActiveVersionRequest'
  { _tavrVersion ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemplateActiveVersionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tavrVersion' - The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
templateActiveVersionRequest ::
  TemplateActiveVersionRequest
templateActiveVersionRequest =
  TemplateActiveVersionRequest' {_tavrVersion = Nothing}

-- | The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
tavrVersion :: Lens' TemplateActiveVersionRequest (Maybe Text)
tavrVersion = lens _tavrVersion (\s a -> s {_tavrVersion = a})

instance Hashable TemplateActiveVersionRequest

instance NFData TemplateActiveVersionRequest

instance ToJSON TemplateActiveVersionRequest where
  toJSON TemplateActiveVersionRequest' {..} =
    object (catMaybes [("Version" .=) <$> _tavrVersion])
