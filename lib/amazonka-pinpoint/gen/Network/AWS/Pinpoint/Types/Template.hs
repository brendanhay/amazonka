{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Template
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Template where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the name and version of the message template to use for the message.
--
--
--
-- /See:/ 'template' smart constructor.
data Template = Template'
  { _tName :: !(Maybe Text),
    _tVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Template' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tName' - The name of the message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- * 'tVersion' - The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
template ::
  Template
template = Template' {_tName = Nothing, _tVersion = Nothing}

-- | The name of the message template to use for the message. If specified, this value must match the name of an existing message template.
tName :: Lens' Template (Maybe Text)
tName = lens _tName (\s a -> s {_tName = a})

-- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource. If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
tVersion :: Lens' Template (Maybe Text)
tVersion = lens _tVersion (\s a -> s {_tVersion = a})

instance FromJSON Template where
  parseJSON =
    withObject
      "Template"
      (\x -> Template' <$> (x .:? "Name") <*> (x .:? "Version"))

instance Hashable Template

instance NFData Template

instance ToJSON Template where
  toJSON Template' {..} =
    object
      (catMaybes [("Name" .=) <$> _tName, ("Version" .=) <$> _tVersion])
