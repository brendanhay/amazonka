{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAF.Types.HTTPHeader
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.HTTPHeader where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The response from a GetSampledRequests request includes an @HTTPHeader@
-- complex type that appears as @Headers@ in the response syntax.
-- @HTTPHeader@ contains the names and values of all of the headers that
-- appear in one of the web requests that were returned by
-- @GetSampledRequests@.
--
-- /See:/ 'newHTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { -- | The name of one of the headers in the sampled web request.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of one of the headers in the sampled web request.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HTTPHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'hTTPHeader_name' - The name of one of the headers in the sampled web request.
--
-- 'value', 'hTTPHeader_value' - The value of one of the headers in the sampled web request.
newHTTPHeader ::
  HTTPHeader
newHTTPHeader =
  HTTPHeader'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of one of the headers in the sampled web request.
hTTPHeader_name :: Lens.Lens' HTTPHeader (Prelude.Maybe Prelude.Text)
hTTPHeader_name = Lens.lens (\HTTPHeader' {name} -> name) (\s@HTTPHeader' {} a -> s {name = a} :: HTTPHeader)

-- | The value of one of the headers in the sampled web request.
hTTPHeader_value :: Lens.Lens' HTTPHeader (Prelude.Maybe Prelude.Text)
hTTPHeader_value = Lens.lens (\HTTPHeader' {value} -> value) (\s@HTTPHeader' {} a -> s {value = a} :: HTTPHeader)

instance Prelude.FromJSON HTTPHeader where
  parseJSON =
    Prelude.withObject
      "HTTPHeader"
      ( \x ->
          HTTPHeader'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable HTTPHeader

instance Prelude.NFData HTTPHeader
