{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Template
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Template where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
--
--
--
-- /See:/ 'template' smart constructor.
data Template = Template'
  { _tTextPart :: !(Maybe Text),
    _tSubjectPart :: !(Maybe Text),
    _tHTMLPart :: !(Maybe Text),
    _tTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Template' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTextPart' - The email body that will be visible to recipients whose email clients do not display HTML.
--
-- * 'tSubjectPart' - The subject line of the email.
--
-- * 'tHTMLPart' - The HTML body of the email.
--
-- * 'tTemplateName' - The name of the template. You will refer to this name when you send email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@ operations.
template ::
  -- | 'tTemplateName'
  Text ->
  Template
template pTemplateName_ =
  Template'
    { _tTextPart = Nothing,
      _tSubjectPart = Nothing,
      _tHTMLPart = Nothing,
      _tTemplateName = pTemplateName_
    }

-- | The email body that will be visible to recipients whose email clients do not display HTML.
tTextPart :: Lens' Template (Maybe Text)
tTextPart = lens _tTextPart (\s a -> s {_tTextPart = a})

-- | The subject line of the email.
tSubjectPart :: Lens' Template (Maybe Text)
tSubjectPart = lens _tSubjectPart (\s a -> s {_tSubjectPart = a})

-- | The HTML body of the email.
tHTMLPart :: Lens' Template (Maybe Text)
tHTMLPart = lens _tHTMLPart (\s a -> s {_tHTMLPart = a})

-- | The name of the template. You will refer to this name when you send email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@ operations.
tTemplateName :: Lens' Template Text
tTemplateName = lens _tTemplateName (\s a -> s {_tTemplateName = a})

instance FromXML Template where
  parseXML x =
    Template'
      <$> (x .@? "TextPart")
      <*> (x .@? "SubjectPart")
      <*> (x .@? "HtmlPart")
      <*> (x .@ "TemplateName")

instance Hashable Template

instance NFData Template

instance ToQuery Template where
  toQuery Template' {..} =
    mconcat
      [ "TextPart" =: _tTextPart,
        "SubjectPart" =: _tSubjectPart,
        "HtmlPart" =: _tHTMLPart,
        "TemplateName" =: _tTemplateName
      ]
