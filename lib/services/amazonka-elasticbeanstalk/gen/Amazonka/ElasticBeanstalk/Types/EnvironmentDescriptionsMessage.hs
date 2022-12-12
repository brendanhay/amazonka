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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.EnvironmentDescription
import qualified Amazonka.Prelude as Prelude

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'newEnvironmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { -- | Returns an EnvironmentDescription list.
    environments :: Prelude.Maybe [EnvironmentDescription],
    -- | In a paginated request, the token that you can pass in a subsequent
    -- request to get the next response page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentDescriptionsMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environments', 'environmentDescriptionsMessage_environments' - Returns an EnvironmentDescription list.
--
-- 'nextToken', 'environmentDescriptionsMessage_nextToken' - In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
newEnvironmentDescriptionsMessage ::
  EnvironmentDescriptionsMessage
newEnvironmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    { environments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Returns an EnvironmentDescription list.
environmentDescriptionsMessage_environments :: Lens.Lens' EnvironmentDescriptionsMessage (Prelude.Maybe [EnvironmentDescription])
environmentDescriptionsMessage_environments = Lens.lens (\EnvironmentDescriptionsMessage' {environments} -> environments) (\s@EnvironmentDescriptionsMessage' {} a -> s {environments = a} :: EnvironmentDescriptionsMessage) Prelude.. Lens.mapping Lens.coerced

-- | In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
environmentDescriptionsMessage_nextToken :: Lens.Lens' EnvironmentDescriptionsMessage (Prelude.Maybe Prelude.Text)
environmentDescriptionsMessage_nextToken = Lens.lens (\EnvironmentDescriptionsMessage' {nextToken} -> nextToken) (\s@EnvironmentDescriptionsMessage' {} a -> s {nextToken = a} :: EnvironmentDescriptionsMessage)

instance Data.FromXML EnvironmentDescriptionsMessage where
  parseXML x =
    EnvironmentDescriptionsMessage'
      Prelude.<$> ( x Data..@? "Environments" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "NextToken")

instance
  Prelude.Hashable
    EnvironmentDescriptionsMessage
  where
  hashWithSalt
    _salt
    EnvironmentDescriptionsMessage' {..} =
      _salt `Prelude.hashWithSalt` environments
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    EnvironmentDescriptionsMessage
  where
  rnf EnvironmentDescriptionsMessage' {..} =
    Prelude.rnf environments
      `Prelude.seq` Prelude.rnf nextToken
