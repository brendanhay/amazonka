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
-- Module      : Network.AWS.WAF.Types.WebACLSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WebACLSummary where

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
-- Contains the identifier and the name or description of the WebACL.
--
-- /See:/ 'newWebACLSummary' smart constructor.
data WebACLSummary = WebACLSummary'
  { -- | A unique identifier for a @WebACL@. You use @WebACLId@ to get
    -- information about a @WebACL@ (see GetWebACL), update a @WebACL@ (see
    -- UpdateWebACL), and delete a @WebACL@ from AWS WAF (see DeleteWebACL).
    --
    -- @WebACLId@ is returned by CreateWebACL and by ListWebACLs.
    webACLId :: Prelude.Text,
    -- | A friendly name or description of the WebACL. You can\'t change the name
    -- of a @WebACL@ after you create it.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WebACLSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLId', 'webACLSummary_webACLId' - A unique identifier for a @WebACL@. You use @WebACLId@ to get
-- information about a @WebACL@ (see GetWebACL), update a @WebACL@ (see
-- UpdateWebACL), and delete a @WebACL@ from AWS WAF (see DeleteWebACL).
--
-- @WebACLId@ is returned by CreateWebACL and by ListWebACLs.
--
-- 'name', 'webACLSummary_name' - A friendly name or description of the WebACL. You can\'t change the name
-- of a @WebACL@ after you create it.
newWebACLSummary ::
  -- | 'webACLId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  WebACLSummary
newWebACLSummary pWebACLId_ pName_ =
  WebACLSummary'
    { webACLId = pWebACLId_,
      name = pName_
    }

-- | A unique identifier for a @WebACL@. You use @WebACLId@ to get
-- information about a @WebACL@ (see GetWebACL), update a @WebACL@ (see
-- UpdateWebACL), and delete a @WebACL@ from AWS WAF (see DeleteWebACL).
--
-- @WebACLId@ is returned by CreateWebACL and by ListWebACLs.
webACLSummary_webACLId :: Lens.Lens' WebACLSummary Prelude.Text
webACLSummary_webACLId = Lens.lens (\WebACLSummary' {webACLId} -> webACLId) (\s@WebACLSummary' {} a -> s {webACLId = a} :: WebACLSummary)

-- | A friendly name or description of the WebACL. You can\'t change the name
-- of a @WebACL@ after you create it.
webACLSummary_name :: Lens.Lens' WebACLSummary Prelude.Text
webACLSummary_name = Lens.lens (\WebACLSummary' {name} -> name) (\s@WebACLSummary' {} a -> s {name = a} :: WebACLSummary)

instance Prelude.FromJSON WebACLSummary where
  parseJSON =
    Prelude.withObject
      "WebACLSummary"
      ( \x ->
          WebACLSummary'
            Prelude.<$> (x Prelude..: "WebACLId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable WebACLSummary

instance Prelude.NFData WebACLSummary
