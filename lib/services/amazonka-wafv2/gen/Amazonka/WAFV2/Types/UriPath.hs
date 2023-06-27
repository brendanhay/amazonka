{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.Types.UriPath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.UriPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Inspect the path component of the URI of the web request. This is the
-- part of the web request that identifies a resource. For example,
-- @\/images\/daily-ad.jpg@.
--
-- This is used in the FieldToMatch specification for some web request
-- component types.
--
-- JSON specification: @\"UriPath\": {}@
--
-- /See:/ 'newUriPath' smart constructor.
data UriPath = UriPath'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UriPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUriPath ::
  UriPath
newUriPath = UriPath'

instance Data.FromJSON UriPath where
  parseJSON =
    Data.withObject
      "UriPath"
      (\x -> Prelude.pure UriPath')

instance Prelude.Hashable UriPath where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData UriPath where
  rnf _ = ()

instance Data.ToJSON UriPath where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
