{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeBatch where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Change

-- | The information for a change request.
--
--
--
-- /See:/ 'changeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
  { _cbComment :: !(Maybe Text),
    _cbChanges :: !(List1 Change)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChangeBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbComment' - /Optional:/ Any comments you want to include about a change batch request.
--
-- * 'cbChanges' - Information about the changes to make to the record sets.
changeBatch ::
  -- | 'cbChanges'
  NonEmpty Change ->
  ChangeBatch
changeBatch pChanges_ =
  ChangeBatch'
    { _cbComment = Nothing,
      _cbChanges = _List1 # pChanges_
    }

-- | /Optional:/ Any comments you want to include about a change batch request.
cbComment :: Lens' ChangeBatch (Maybe Text)
cbComment = lens _cbComment (\s a -> s {_cbComment = a})

-- | Information about the changes to make to the record sets.
cbChanges :: Lens' ChangeBatch (NonEmpty Change)
cbChanges = lens _cbChanges (\s a -> s {_cbChanges = a}) . _List1

instance Hashable ChangeBatch

instance NFData ChangeBatch

instance ToXML ChangeBatch where
  toXML ChangeBatch' {..} =
    mconcat
      [ "Comment" @= _cbComment,
        "Changes" @= toXMLList "Change" _cbChanges
      ]
