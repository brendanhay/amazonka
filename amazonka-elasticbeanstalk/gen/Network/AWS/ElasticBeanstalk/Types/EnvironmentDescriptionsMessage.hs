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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage where

import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'newEnvironmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { -- | In a paginated request, the token that you can pass in a subsequent
    -- request to get the next response page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns an EnvironmentDescription list.
    environments :: Prelude.Maybe [EnvironmentDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentDescriptionsMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'environmentDescriptionsMessage_nextToken' - In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
--
-- 'environments', 'environmentDescriptionsMessage_environments' - Returns an EnvironmentDescription list.
newEnvironmentDescriptionsMessage ::
  EnvironmentDescriptionsMessage
newEnvironmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    { nextToken =
        Prelude.Nothing,
      environments = Prelude.Nothing
    }

-- | In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
environmentDescriptionsMessage_nextToken :: Lens.Lens' EnvironmentDescriptionsMessage (Prelude.Maybe Prelude.Text)
environmentDescriptionsMessage_nextToken = Lens.lens (\EnvironmentDescriptionsMessage' {nextToken} -> nextToken) (\s@EnvironmentDescriptionsMessage' {} a -> s {nextToken = a} :: EnvironmentDescriptionsMessage)

-- | Returns an EnvironmentDescription list.
environmentDescriptionsMessage_environments :: Lens.Lens' EnvironmentDescriptionsMessage (Prelude.Maybe [EnvironmentDescription])
environmentDescriptionsMessage_environments = Lens.lens (\EnvironmentDescriptionsMessage' {environments} -> environments) (\s@EnvironmentDescriptionsMessage' {} a -> s {environments = a} :: EnvironmentDescriptionsMessage) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    EnvironmentDescriptionsMessage
  where
  parseXML x =
    EnvironmentDescriptionsMessage'
      Prelude.<$> (x Prelude..@? "NextToken")
      Prelude.<*> ( x Prelude..@? "Environments"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    EnvironmentDescriptionsMessage

instance
  Prelude.NFData
    EnvironmentDescriptionsMessage
