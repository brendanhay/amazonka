{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnsuccessfulItemError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about items that were not successfully processed in a batch call.
--
--
--
-- /See:/ 'unsuccessfulItem' smart constructor.
data UnsuccessfulItem = UnsuccessfulItem'
  { _uiResourceId ::
      !(Maybe Text),
    _uiError :: !(Maybe UnsuccessfulItemError)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnsuccessfulItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiResourceId' - The ID of the resource.
--
-- * 'uiError' - Information about the error.
unsuccessfulItem ::
  UnsuccessfulItem
unsuccessfulItem =
  UnsuccessfulItem' {_uiResourceId = Nothing, _uiError = Nothing}

-- | The ID of the resource.
uiResourceId :: Lens' UnsuccessfulItem (Maybe Text)
uiResourceId = lens _uiResourceId (\s a -> s {_uiResourceId = a})

-- | Information about the error.
uiError :: Lens' UnsuccessfulItem (Maybe UnsuccessfulItemError)
uiError = lens _uiError (\s a -> s {_uiError = a})

instance FromXML UnsuccessfulItem where
  parseXML x =
    UnsuccessfulItem' <$> (x .@? "resourceId") <*> (x .@? "error")

instance Hashable UnsuccessfulItem

instance NFData UnsuccessfulItem
