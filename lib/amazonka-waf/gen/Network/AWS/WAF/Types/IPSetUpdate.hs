{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.IPSetDescriptor

-- | Specifies the type of update to perform to an 'IPSet' with 'UpdateIPSet' .
--
--
--
-- /See:/ 'ipSetUpdate' smart constructor.
data IPSetUpdate = IPSetUpdate'
  { _isuAction :: !ChangeAction,
    _isuIPSetDescriptor :: !IPSetDescriptor
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isuAction' - Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
--
-- * 'isuIPSetDescriptor' - The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
ipSetUpdate ::
  -- | 'isuAction'
  ChangeAction ->
  -- | 'isuIPSetDescriptor'
  IPSetDescriptor ->
  IPSetUpdate
ipSetUpdate pAction_ pIPSetDescriptor_ =
  IPSetUpdate'
    { _isuAction = pAction_,
      _isuIPSetDescriptor = pIPSetDescriptor_
    }

-- | Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
isuAction :: Lens' IPSetUpdate ChangeAction
isuAction = lens _isuAction (\s a -> s {_isuAction = a})

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
isuIPSetDescriptor :: Lens' IPSetUpdate IPSetDescriptor
isuIPSetDescriptor = lens _isuIPSetDescriptor (\s a -> s {_isuIPSetDescriptor = a})

instance Hashable IPSetUpdate

instance NFData IPSetUpdate

instance ToJSON IPSetUpdate where
  toJSON IPSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _isuAction),
            Just ("IPSetDescriptor" .= _isuIPSetDescriptor)
          ]
      )
