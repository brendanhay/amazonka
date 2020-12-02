{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Content
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Content where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents textual data, plus an optional character set specification.
--
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.
--
--
-- /See:/ 'content' smart constructor.
data Content = Content'
  { _cCharset :: !(Maybe Text),
    _cData :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Content' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCharset' - The character set of the content.
--
-- * 'cData' - The textual data of the content.
content ::
  -- | 'cData'
  Text ->
  Content
content pData_ = Content' {_cCharset = Nothing, _cData = pData_}

-- | The character set of the content.
cCharset :: Lens' Content (Maybe Text)
cCharset = lens _cCharset (\s a -> s {_cCharset = a})

-- | The textual data of the content.
cData :: Lens' Content Text
cData = lens _cData (\s a -> s {_cData = a})

instance Hashable Content

instance NFData Content

instance ToQuery Content where
  toQuery Content' {..} =
    mconcat ["Charset" =: _cCharset, "Data" =: _cData]
