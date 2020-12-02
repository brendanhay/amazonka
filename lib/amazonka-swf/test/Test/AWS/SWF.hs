{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Test.AWS.SWF
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.SWF
    ( tests
    , fixtures
    ) where

import           Data.Time
import           Network.AWS.Prelude
import           Network.AWS.SWF
import           Test.AWS.Gen.SWF
import           Test.AWS.Prelude

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "response"
        [ responsePollForActivityTask $
            pollForActivityTaskResponse 200 11
                & pfatrsWorkflowExecution ?~ workflowExecution "20110927-T-1" "cfa2bd33-31b0-4b75-b131-255bb0d97b3f"
                & pfatrsActivityType      ?~ activityType "activityVerify" "1.0"
                & pfatrsInput             ?~ "5634-0056-4367-0923,12/12,437"
                & pfatrsActivityId        ?~ "verification-27"
                & pfatrsTaskToken         ?~
                    "AAAAKgAAAAEAAAAAAAAAATZDvCYwk/hP/X1ZGdJhb+T6OWzcBx2DPhsIi5HF4aGQI4OXrDE7Ny3uM+aiAhGrmeNyVAa4yNIBQuoZuJA5G+BoaB0JuHFBOynHDTnm7ayNH43KhMkfdrDG4elfHSz3m/EtbLnFGueAr7+3NKDG6x4sTKg3cZpOtSguSx05yI1X3AtscS8ATcLB2Y3Aub1YonN/i/k67voca/GFsSiwSz3AAnJj1IPvrujgIj9KUvckwRPC5ET7d33XJcRp+gHYzZsBLVBaRmV3gEYAnp2ICslFn4YSjGy+dFXCNpOa4G1O8pczCbFUGbQ3+5wf0RSaa/xMq2pfdBKnuFp0wp8kw1k+5ZsbtDZeZn8g5GyKCLiLms/xD0OxugGGUe5ZlAoHEkTWGxZj/G32P7cMoCgrcACfFPdx1LNYYEre7YiGiyjGnfW2t5mW7VK9Np28vcXVbdpH4JNEB9OuB1xqL8N8ifPVtc72uxB1i9XEdq/8rkXasSEw4TubB2FwgqnuJstmfEhpOdb5HfhR6OwmnHuk9eszO/fUkGucTUXQP2hhB+Gz"

        , responsePollForDecisionTask $
            pollForDecisionTaskResponse 200 3
                & pfdtrsWorkflowExecution      ?~ workflowExecution "20110927-T-1" "06b8f87a-24b3-40b6-9ceb-9676f28e9493"
                & pfdtrsWorkflowType           ?~ workflowType "customerOrderWorkflow" "1.0"
                & pfdtrsTaskToken              ?~
                    "AAAAKgAAAAEAAAAAAAAAATZDvCYwk/hP/X1ZGdJhb+T6OWzcBx2DPhsIi5HF4aGQI4OXrDE7Ny3uM+aiAhGrmeNyVAa4yNIBQuoZuJA5G+BoaB0JuHFBOynHDTnm7ayNH43KhMkfdrDG4elfHSz3m/EtbLnFGueAr7+3NKDG6x4sTKg3cZpOtSguSx05yI1X3AtscS8ATcLB2Y3Aub1YonN/i/k67voca/GFsSiwSz3AAnJj1IPvrujgIj9KUvckwRPC5ET7d33XJcRp+gHYzZsBLVBaRmV3gEYAnp2ICslFn4YSjGy+dFXCNpOa4G1O8pczCbFUGbQ3+5wf0RSaa/xMq2pfdBKnuFp0wp8kw1k+5ZsbtDZeZn8g5GyKCLiLms/xD0OxugGGUe5ZlAoHEkTWGxZj/G32P7cMoCgrcACfFPdx1LNYYEre7YiGiyjGnfW2t5mW7VK9Np28vcXVbdpH4JNEB9OuB1xqL8N8ifPVtc72uxB1i9XEdq/8rkXasSEw4TubB2FwgqnuJstmfEhpOdb5HfhR6OwmnHuk9eszO/fUkGucTUXQP2hhB+Gz"
                & pfdtrsPreviousStartedEventId ?~ 0
                & pfdtrsEvents                 .~
                    [ historyEvent $(mkTime "2012-01-15T03:09:54.566+01:00") DecisionTaskStarted 3
                        & heDecisionTaskStartedEventAttributes ?~
                             (decisionTaskStartedEventAttributes 2
                                 & dtseaIdentity ?~ "Decider01")

                    , historyEvent $(mkTime "2012-01-15T02:56:59.474+01:00") WorkflowExecutionStarted 1
                        & heWorkflowExecutionStartedEventAttributes ?~
                            (workflowExecutionStartedEventAttributes
                                Terminate
                                (taskList "specialTaskList")
                                (workflowType "customerOrderWorkflow" "1.0")
                                & weseaTaskStartToCloseTimeout      ?~ "600"
                                & weseaExecutionStartToCloseTimeout ?~ "3600"
                                & weseaParentInitiatedEventId       ?~ 0
                                & weseaInput                        ?~ "arbitrary-string-that-is-meaningful-to-the-workflow"
                                & weseaTagList                      .~
                                    [ "music purchase"
                                    , "digital"
                                    , "ricoh-the-dog"
                                    ])
                    ]
        ]
    ]
