{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a resize operation.
--
--
--
-- /See:/ 'resizeInfo' smart constructor.
data ResizeInfo = ResizeInfo'
  { _riAllowCancelResize ::
      !(Maybe Bool),
    _riResizeType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResizeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riAllowCancelResize' - A boolean value indicating if the resize operation can be cancelled.
--
-- * 'riResizeType' - Returns the value @ClassicResize@ .
resizeInfo ::
  ResizeInfo
resizeInfo =
  ResizeInfo'
    { _riAllowCancelResize = Nothing,
      _riResizeType = Nothing
    }

-- | A boolean value indicating if the resize operation can be cancelled.
riAllowCancelResize :: Lens' ResizeInfo (Maybe Bool)
riAllowCancelResize = lens _riAllowCancelResize (\s a -> s {_riAllowCancelResize = a})

-- | Returns the value @ClassicResize@ .
riResizeType :: Lens' ResizeInfo (Maybe Text)
riResizeType = lens _riResizeType (\s a -> s {_riResizeType = a})

instance FromXML ResizeInfo where
  parseXML x =
    ResizeInfo'
      <$> (x .@? "AllowCancelResize") <*> (x .@? "ResizeType")

instance Hashable ResizeInfo

instance NFData ResizeInfo
