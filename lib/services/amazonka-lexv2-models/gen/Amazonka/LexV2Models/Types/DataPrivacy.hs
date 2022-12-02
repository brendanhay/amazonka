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
-- Module      : Amazonka.LexV2Models.Types.DataPrivacy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DataPrivacy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | By default, data stored by Amazon Lex is encrypted. The @DataPrivacy@
-- structure provides settings that determine how Amazon Lex handles
-- special cases of securing the data for your bot.
--
-- /See:/ 'newDataPrivacy' smart constructor.
data DataPrivacy = DataPrivacy'
  { -- | For each Amazon Lex bot created with the Amazon Lex Model Building
    -- Service, you must specify whether your use of Amazon Lex is related to a
    -- website, program, or other application that is directed or targeted, in
    -- whole or in part, to children under age 13 and subject to the
    -- Children\'s Online Privacy Protection Act (COPPA) by specifying @true@
    -- or @false@ in the @childDirected@ field. By specifying @true@ in the
    -- @childDirected@ field, you confirm that your use of Amazon Lex __is__
    -- related to a website, program, or other application that is directed or
    -- targeted, in whole or in part, to children under age 13 and subject to
    -- COPPA. By specifying @false@ in the @childDirected@ field, you confirm
    -- that your use of Amazon Lex __is not__ related to a website, program, or
    -- other application that is directed or targeted, in whole or in part, to
    -- children under age 13 and subject to COPPA. You may not specify a
    -- default value for the @childDirected@ field that does not accurately
    -- reflect whether your use of Amazon Lex is related to a website, program,
    -- or other application that is directed or targeted, in whole or in part,
    -- to children under age 13 and subject to COPPA. If your use of Amazon Lex
    -- relates to a website, program, or other application that is directed in
    -- whole or in part, to children under age 13, you must obtain any required
    -- verifiable parental consent under COPPA. For information regarding the
    -- use of Amazon Lex in connection with websites, programs, or other
    -- applications that are directed or targeted, in whole or in part, to
    -- children under age 13, see the
    -- <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ>.
    childDirected :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPrivacy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childDirected', 'dataPrivacy_childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building
-- Service, you must specify whether your use of Amazon Lex is related to a
-- website, program, or other application that is directed or targeted, in
-- whole or in part, to children under age 13 and subject to the
-- Children\'s Online Privacy Protection Act (COPPA) by specifying @true@
-- or @false@ in the @childDirected@ field. By specifying @true@ in the
-- @childDirected@ field, you confirm that your use of Amazon Lex __is__
-- related to a website, program, or other application that is directed or
-- targeted, in whole or in part, to children under age 13 and subject to
-- COPPA. By specifying @false@ in the @childDirected@ field, you confirm
-- that your use of Amazon Lex __is not__ related to a website, program, or
-- other application that is directed or targeted, in whole or in part, to
-- children under age 13 and subject to COPPA. You may not specify a
-- default value for the @childDirected@ field that does not accurately
-- reflect whether your use of Amazon Lex is related to a website, program,
-- or other application that is directed or targeted, in whole or in part,
-- to children under age 13 and subject to COPPA. If your use of Amazon Lex
-- relates to a website, program, or other application that is directed in
-- whole or in part, to children under age 13, you must obtain any required
-- verifiable parental consent under COPPA. For information regarding the
-- use of Amazon Lex in connection with websites, programs, or other
-- applications that are directed or targeted, in whole or in part, to
-- children under age 13, see the
-- <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ>.
newDataPrivacy ::
  -- | 'childDirected'
  Prelude.Bool ->
  DataPrivacy
newDataPrivacy pChildDirected_ =
  DataPrivacy' {childDirected = pChildDirected_}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building
-- Service, you must specify whether your use of Amazon Lex is related to a
-- website, program, or other application that is directed or targeted, in
-- whole or in part, to children under age 13 and subject to the
-- Children\'s Online Privacy Protection Act (COPPA) by specifying @true@
-- or @false@ in the @childDirected@ field. By specifying @true@ in the
-- @childDirected@ field, you confirm that your use of Amazon Lex __is__
-- related to a website, program, or other application that is directed or
-- targeted, in whole or in part, to children under age 13 and subject to
-- COPPA. By specifying @false@ in the @childDirected@ field, you confirm
-- that your use of Amazon Lex __is not__ related to a website, program, or
-- other application that is directed or targeted, in whole or in part, to
-- children under age 13 and subject to COPPA. You may not specify a
-- default value for the @childDirected@ field that does not accurately
-- reflect whether your use of Amazon Lex is related to a website, program,
-- or other application that is directed or targeted, in whole or in part,
-- to children under age 13 and subject to COPPA. If your use of Amazon Lex
-- relates to a website, program, or other application that is directed in
-- whole or in part, to children under age 13, you must obtain any required
-- verifiable parental consent under COPPA. For information regarding the
-- use of Amazon Lex in connection with websites, programs, or other
-- applications that are directed or targeted, in whole or in part, to
-- children under age 13, see the
-- <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ>.
dataPrivacy_childDirected :: Lens.Lens' DataPrivacy Prelude.Bool
dataPrivacy_childDirected = Lens.lens (\DataPrivacy' {childDirected} -> childDirected) (\s@DataPrivacy' {} a -> s {childDirected = a} :: DataPrivacy)

instance Data.FromJSON DataPrivacy where
  parseJSON =
    Data.withObject
      "DataPrivacy"
      ( \x ->
          DataPrivacy' Prelude.<$> (x Data..: "childDirected")
      )

instance Prelude.Hashable DataPrivacy where
  hashWithSalt _salt DataPrivacy' {..} =
    _salt `Prelude.hashWithSalt` childDirected

instance Prelude.NFData DataPrivacy where
  rnf DataPrivacy' {..} = Prelude.rnf childDirected

instance Data.ToJSON DataPrivacy where
  toJSON DataPrivacy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("childDirected" Data..= childDirected)
          ]
      )
