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
-- Module      : Network.AWS.WAF.Types.WebACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WebACL where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.WafAction

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Contains the @Rules@ that identify the requests that you want to allow,
-- block, or count. In a @WebACL@, you also specify a default action
-- (@ALLOW@ or @BLOCK@), and the action for each @Rule@ that you add to a
-- @WebACL@, for example, block requests from specified IP addresses or
-- block requests from specified referrers. You also associate the @WebACL@
-- with a CloudFront distribution to identify the requests that you want
-- AWS WAF to filter. If you add more than one @Rule@ to a @WebACL@, a
-- request needs to match only one of the specifications to be allowed,
-- blocked, or counted. For more information, see UpdateWebACL.
--
-- /See:/ 'newWebACL' smart constructor.
data WebACL = WebACL'
  { -- | A friendly name or description for the metrics for this @WebACL@. The
    -- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
    -- maximum length 128 and minimum length one. It can\'t contain whitespace
    -- or metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change @MetricName@ after you create the
    -- @WebACL@.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | Tha Amazon Resource Name (ARN) of the web ACL.
    webACLArn :: Prelude.Maybe Prelude.Text,
    -- | A friendly name or description of the @WebACL@. You can\'t change the
    -- name of a @WebACL@ after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a @WebACL@. You use @WebACLId@ to get
    -- information about a @WebACL@ (see GetWebACL), update a @WebACL@ (see
    -- UpdateWebACL), and delete a @WebACL@ from AWS WAF (see DeleteWebACL).
    --
    -- @WebACLId@ is returned by CreateWebACL and by ListWebACLs.
    webACLId :: Prelude.Text,
    -- | The action to perform if none of the @Rules@ contained in the @WebACL@
    -- match. The action is specified by the WafAction object.
    defaultAction :: WafAction,
    -- | An array that contains the action for each @Rule@ in a @WebACL@, the
    -- priority of the @Rule@, and the ID of the @Rule@.
    rules :: [ActivatedRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'webACL_metricName' - A friendly name or description for the metrics for this @WebACL@. The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change @MetricName@ after you create the
-- @WebACL@.
--
-- 'webACLArn', 'webACL_webACLArn' - Tha Amazon Resource Name (ARN) of the web ACL.
--
-- 'name', 'webACL_name' - A friendly name or description of the @WebACL@. You can\'t change the
-- name of a @WebACL@ after you create it.
--
-- 'webACLId', 'webACL_webACLId' - A unique identifier for a @WebACL@. You use @WebACLId@ to get
-- information about a @WebACL@ (see GetWebACL), update a @WebACL@ (see
-- UpdateWebACL), and delete a @WebACL@ from AWS WAF (see DeleteWebACL).
--
-- @WebACLId@ is returned by CreateWebACL and by ListWebACLs.
--
-- 'defaultAction', 'webACL_defaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@
-- match. The action is specified by the WafAction object.
--
-- 'rules', 'webACL_rules' - An array that contains the action for each @Rule@ in a @WebACL@, the
-- priority of the @Rule@, and the ID of the @Rule@.
newWebACL ::
  -- | 'webACLId'
  Prelude.Text ->
  -- | 'defaultAction'
  WafAction ->
  WebACL
newWebACL pWebACLId_ pDefaultAction_ =
  WebACL'
    { metricName = Prelude.Nothing,
      webACLArn = Prelude.Nothing,
      name = Prelude.Nothing,
      webACLId = pWebACLId_,
      defaultAction = pDefaultAction_,
      rules = Prelude.mempty
    }

-- | A friendly name or description for the metrics for this @WebACL@. The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change @MetricName@ after you create the
-- @WebACL@.
webACL_metricName :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Text)
webACL_metricName = Lens.lens (\WebACL' {metricName} -> metricName) (\s@WebACL' {} a -> s {metricName = a} :: WebACL)

-- | Tha Amazon Resource Name (ARN) of the web ACL.
webACL_webACLArn :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Text)
webACL_webACLArn = Lens.lens (\WebACL' {webACLArn} -> webACLArn) (\s@WebACL' {} a -> s {webACLArn = a} :: WebACL)

-- | A friendly name or description of the @WebACL@. You can\'t change the
-- name of a @WebACL@ after you create it.
webACL_name :: Lens.Lens' WebACL (Prelude.Maybe Prelude.Text)
webACL_name = Lens.lens (\WebACL' {name} -> name) (\s@WebACL' {} a -> s {name = a} :: WebACL)

-- | A unique identifier for a @WebACL@. You use @WebACLId@ to get
-- information about a @WebACL@ (see GetWebACL), update a @WebACL@ (see
-- UpdateWebACL), and delete a @WebACL@ from AWS WAF (see DeleteWebACL).
--
-- @WebACLId@ is returned by CreateWebACL and by ListWebACLs.
webACL_webACLId :: Lens.Lens' WebACL Prelude.Text
webACL_webACLId = Lens.lens (\WebACL' {webACLId} -> webACLId) (\s@WebACL' {} a -> s {webACLId = a} :: WebACL)

-- | The action to perform if none of the @Rules@ contained in the @WebACL@
-- match. The action is specified by the WafAction object.
webACL_defaultAction :: Lens.Lens' WebACL WafAction
webACL_defaultAction = Lens.lens (\WebACL' {defaultAction} -> defaultAction) (\s@WebACL' {} a -> s {defaultAction = a} :: WebACL)

-- | An array that contains the action for each @Rule@ in a @WebACL@, the
-- priority of the @Rule@, and the ID of the @Rule@.
webACL_rules :: Lens.Lens' WebACL [ActivatedRule]
webACL_rules = Lens.lens (\WebACL' {rules} -> rules) (\s@WebACL' {} a -> s {rules = a} :: WebACL) Prelude.. Prelude._Coerce

instance Prelude.FromJSON WebACL where
  parseJSON =
    Prelude.withObject
      "WebACL"
      ( \x ->
          WebACL'
            Prelude.<$> (x Prelude..:? "MetricName")
            Prelude.<*> (x Prelude..:? "WebACLArn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "WebACLId")
            Prelude.<*> (x Prelude..: "DefaultAction")
            Prelude.<*> (x Prelude..:? "Rules" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable WebACL

instance Prelude.NFData WebACL
