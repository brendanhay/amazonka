{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexFieldStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexFieldStatus where

import Network.AWS.CloudSearch.Types.IndexField
import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The value of an @IndexField@ and its current status.
--
--
--
-- /See:/ 'indexFieldStatus' smart constructor.
data IndexFieldStatus = IndexFieldStatus'
  { _ifsOptions ::
      !IndexField,
    _ifsStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IndexFieldStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifsOptions' - Undocumented member.
--
-- * 'ifsStatus' - Undocumented member.
indexFieldStatus ::
  -- | 'ifsOptions'
  IndexField ->
  -- | 'ifsStatus'
  OptionStatus ->
  IndexFieldStatus
indexFieldStatus pOptions_ pStatus_ =
  IndexFieldStatus' {_ifsOptions = pOptions_, _ifsStatus = pStatus_}

-- | Undocumented member.
ifsOptions :: Lens' IndexFieldStatus IndexField
ifsOptions = lens _ifsOptions (\s a -> s {_ifsOptions = a})

-- | Undocumented member.
ifsStatus :: Lens' IndexFieldStatus OptionStatus
ifsStatus = lens _ifsStatus (\s a -> s {_ifsStatus = a})

instance FromXML IndexFieldStatus where
  parseXML x =
    IndexFieldStatus' <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable IndexFieldStatus

instance NFData IndexFieldStatus
