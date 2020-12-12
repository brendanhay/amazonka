{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberCountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberCountryCode
  ( PhoneNumberCountryCode
      ( PhoneNumberCountryCode',
        PNCCAD,
        PNCCAE,
        PNCCAF,
        PNCCAG,
        PNCCAI,
        PNCCAL,
        PNCCAM,
        PNCCAN,
        PNCCAO,
        PNCCAQ,
        PNCCAR,
        PNCCAS,
        PNCCAT,
        PNCCAU,
        PNCCAW,
        PNCCAZ,
        PNCCBA,
        PNCCBB,
        PNCCBD,
        PNCCBE,
        PNCCBF,
        PNCCBG,
        PNCCBH,
        PNCCBI,
        PNCCBJ,
        PNCCBL,
        PNCCBM,
        PNCCBN,
        PNCCBO,
        PNCCBR,
        PNCCBS,
        PNCCBT,
        PNCCBW,
        PNCCBY,
        PNCCBZ,
        PNCCCA,
        PNCCCC,
        PNCCCD,
        PNCCCF,
        PNCCCG,
        PNCCCH,
        PNCCCI,
        PNCCCK,
        PNCCCL,
        PNCCCM,
        PNCCCN,
        PNCCCO,
        PNCCCR,
        PNCCCU,
        PNCCCV,
        PNCCCW,
        PNCCCX,
        PNCCCY,
        PNCCCZ,
        PNCCDE,
        PNCCDJ,
        PNCCDK,
        PNCCDM,
        PNCCDO,
        PNCCDZ,
        PNCCEC,
        PNCCEE,
        PNCCEG,
        PNCCEH,
        PNCCER,
        PNCCES,
        PNCCET,
        PNCCFI,
        PNCCFJ,
        PNCCFK,
        PNCCFM,
        PNCCFO,
        PNCCFR,
        PNCCGA,
        PNCCGB,
        PNCCGD,
        PNCCGE,
        PNCCGG,
        PNCCGH,
        PNCCGI,
        PNCCGL,
        PNCCGM,
        PNCCGN,
        PNCCGQ,
        PNCCGR,
        PNCCGT,
        PNCCGU,
        PNCCGW,
        PNCCGY,
        PNCCHK,
        PNCCHN,
        PNCCHR,
        PNCCHT,
        PNCCHU,
        PNCCIE,
        PNCCIL,
        PNCCIM,
        PNCCIN,
        PNCCIO,
        PNCCIQ,
        PNCCIR,
        PNCCIS,
        PNCCIT,
        PNCCId,
        PNCCJE,
        PNCCJM,
        PNCCJO,
        PNCCJP,
        PNCCKE,
        PNCCKG,
        PNCCKH,
        PNCCKI,
        PNCCKM,
        PNCCKN,
        PNCCKP,
        PNCCKR,
        PNCCKW,
        PNCCKY,
        PNCCKZ,
        PNCCLA,
        PNCCLB,
        PNCCLC,
        PNCCLI,
        PNCCLK,
        PNCCLR,
        PNCCLS,
        PNCCLT,
        PNCCLU,
        PNCCLV,
        PNCCLY,
        PNCCMA,
        PNCCMC,
        PNCCMD,
        PNCCME,
        PNCCMF,
        PNCCMG,
        PNCCMH,
        PNCCMK,
        PNCCML,
        PNCCMM,
        PNCCMN,
        PNCCMO,
        PNCCMP,
        PNCCMR,
        PNCCMS,
        PNCCMT,
        PNCCMU,
        PNCCMV,
        PNCCMW,
        PNCCMX,
        PNCCMY,
        PNCCMZ,
        PNCCNA,
        PNCCNC,
        PNCCNE,
        PNCCNG,
        PNCCNI,
        PNCCNL,
        PNCCNO,
        PNCCNP,
        PNCCNR,
        PNCCNU,
        PNCCNZ,
        PNCCOM,
        PNCCPA,
        PNCCPE,
        PNCCPF,
        PNCCPG,
        PNCCPH,
        PNCCPK,
        PNCCPL,
        PNCCPM,
        PNCCPN,
        PNCCPR,
        PNCCPT,
        PNCCPW,
        PNCCPY,
        PNCCQA,
        PNCCRE,
        PNCCRO,
        PNCCRS,
        PNCCRU,
        PNCCRW,
        PNCCSA,
        PNCCSB,
        PNCCSC,
        PNCCSD,
        PNCCSE,
        PNCCSG,
        PNCCSH,
        PNCCSI,
        PNCCSJ,
        PNCCSK,
        PNCCSL,
        PNCCSM,
        PNCCSN,
        PNCCSO,
        PNCCSR,
        PNCCST,
        PNCCSV,
        PNCCSX,
        PNCCSY,
        PNCCSZ,
        PNCCTC,
        PNCCTD,
        PNCCTG,
        PNCCTH,
        PNCCTJ,
        PNCCTK,
        PNCCTL,
        PNCCTM,
        PNCCTN,
        PNCCTO,
        PNCCTR,
        PNCCTT,
        PNCCTV,
        PNCCTW,
        PNCCTZ,
        PNCCUA,
        PNCCUG,
        PNCCUS,
        PNCCUY,
        PNCCUZ,
        PNCCVA,
        PNCCVC,
        PNCCVE,
        PNCCVG,
        PNCCVI,
        PNCCVN,
        PNCCVU,
        PNCCWF,
        PNCCWS,
        PNCCYE,
        PNCCYT,
        PNCCZA,
        PNCCZM,
        PNCCZW
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PhoneNumberCountryCode = PhoneNumberCountryCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PNCCAD :: PhoneNumberCountryCode
pattern PNCCAD = PhoneNumberCountryCode' "AD"

pattern PNCCAE :: PhoneNumberCountryCode
pattern PNCCAE = PhoneNumberCountryCode' "AE"

pattern PNCCAF :: PhoneNumberCountryCode
pattern PNCCAF = PhoneNumberCountryCode' "AF"

pattern PNCCAG :: PhoneNumberCountryCode
pattern PNCCAG = PhoneNumberCountryCode' "AG"

pattern PNCCAI :: PhoneNumberCountryCode
pattern PNCCAI = PhoneNumberCountryCode' "AI"

pattern PNCCAL :: PhoneNumberCountryCode
pattern PNCCAL = PhoneNumberCountryCode' "AL"

pattern PNCCAM :: PhoneNumberCountryCode
pattern PNCCAM = PhoneNumberCountryCode' "AM"

pattern PNCCAN :: PhoneNumberCountryCode
pattern PNCCAN = PhoneNumberCountryCode' "AN"

pattern PNCCAO :: PhoneNumberCountryCode
pattern PNCCAO = PhoneNumberCountryCode' "AO"

pattern PNCCAQ :: PhoneNumberCountryCode
pattern PNCCAQ = PhoneNumberCountryCode' "AQ"

pattern PNCCAR :: PhoneNumberCountryCode
pattern PNCCAR = PhoneNumberCountryCode' "AR"

pattern PNCCAS :: PhoneNumberCountryCode
pattern PNCCAS = PhoneNumberCountryCode' "AS"

pattern PNCCAT :: PhoneNumberCountryCode
pattern PNCCAT = PhoneNumberCountryCode' "AT"

pattern PNCCAU :: PhoneNumberCountryCode
pattern PNCCAU = PhoneNumberCountryCode' "AU"

pattern PNCCAW :: PhoneNumberCountryCode
pattern PNCCAW = PhoneNumberCountryCode' "AW"

pattern PNCCAZ :: PhoneNumberCountryCode
pattern PNCCAZ = PhoneNumberCountryCode' "AZ"

pattern PNCCBA :: PhoneNumberCountryCode
pattern PNCCBA = PhoneNumberCountryCode' "BA"

pattern PNCCBB :: PhoneNumberCountryCode
pattern PNCCBB = PhoneNumberCountryCode' "BB"

pattern PNCCBD :: PhoneNumberCountryCode
pattern PNCCBD = PhoneNumberCountryCode' "BD"

pattern PNCCBE :: PhoneNumberCountryCode
pattern PNCCBE = PhoneNumberCountryCode' "BE"

pattern PNCCBF :: PhoneNumberCountryCode
pattern PNCCBF = PhoneNumberCountryCode' "BF"

pattern PNCCBG :: PhoneNumberCountryCode
pattern PNCCBG = PhoneNumberCountryCode' "BG"

pattern PNCCBH :: PhoneNumberCountryCode
pattern PNCCBH = PhoneNumberCountryCode' "BH"

pattern PNCCBI :: PhoneNumberCountryCode
pattern PNCCBI = PhoneNumberCountryCode' "BI"

pattern PNCCBJ :: PhoneNumberCountryCode
pattern PNCCBJ = PhoneNumberCountryCode' "BJ"

pattern PNCCBL :: PhoneNumberCountryCode
pattern PNCCBL = PhoneNumberCountryCode' "BL"

pattern PNCCBM :: PhoneNumberCountryCode
pattern PNCCBM = PhoneNumberCountryCode' "BM"

pattern PNCCBN :: PhoneNumberCountryCode
pattern PNCCBN = PhoneNumberCountryCode' "BN"

pattern PNCCBO :: PhoneNumberCountryCode
pattern PNCCBO = PhoneNumberCountryCode' "BO"

pattern PNCCBR :: PhoneNumberCountryCode
pattern PNCCBR = PhoneNumberCountryCode' "BR"

pattern PNCCBS :: PhoneNumberCountryCode
pattern PNCCBS = PhoneNumberCountryCode' "BS"

pattern PNCCBT :: PhoneNumberCountryCode
pattern PNCCBT = PhoneNumberCountryCode' "BT"

pattern PNCCBW :: PhoneNumberCountryCode
pattern PNCCBW = PhoneNumberCountryCode' "BW"

pattern PNCCBY :: PhoneNumberCountryCode
pattern PNCCBY = PhoneNumberCountryCode' "BY"

pattern PNCCBZ :: PhoneNumberCountryCode
pattern PNCCBZ = PhoneNumberCountryCode' "BZ"

pattern PNCCCA :: PhoneNumberCountryCode
pattern PNCCCA = PhoneNumberCountryCode' "CA"

pattern PNCCCC :: PhoneNumberCountryCode
pattern PNCCCC = PhoneNumberCountryCode' "CC"

pattern PNCCCD :: PhoneNumberCountryCode
pattern PNCCCD = PhoneNumberCountryCode' "CD"

pattern PNCCCF :: PhoneNumberCountryCode
pattern PNCCCF = PhoneNumberCountryCode' "CF"

pattern PNCCCG :: PhoneNumberCountryCode
pattern PNCCCG = PhoneNumberCountryCode' "CG"

pattern PNCCCH :: PhoneNumberCountryCode
pattern PNCCCH = PhoneNumberCountryCode' "CH"

pattern PNCCCI :: PhoneNumberCountryCode
pattern PNCCCI = PhoneNumberCountryCode' "CI"

pattern PNCCCK :: PhoneNumberCountryCode
pattern PNCCCK = PhoneNumberCountryCode' "CK"

pattern PNCCCL :: PhoneNumberCountryCode
pattern PNCCCL = PhoneNumberCountryCode' "CL"

pattern PNCCCM :: PhoneNumberCountryCode
pattern PNCCCM = PhoneNumberCountryCode' "CM"

pattern PNCCCN :: PhoneNumberCountryCode
pattern PNCCCN = PhoneNumberCountryCode' "CN"

pattern PNCCCO :: PhoneNumberCountryCode
pattern PNCCCO = PhoneNumberCountryCode' "CO"

pattern PNCCCR :: PhoneNumberCountryCode
pattern PNCCCR = PhoneNumberCountryCode' "CR"

pattern PNCCCU :: PhoneNumberCountryCode
pattern PNCCCU = PhoneNumberCountryCode' "CU"

pattern PNCCCV :: PhoneNumberCountryCode
pattern PNCCCV = PhoneNumberCountryCode' "CV"

pattern PNCCCW :: PhoneNumberCountryCode
pattern PNCCCW = PhoneNumberCountryCode' "CW"

pattern PNCCCX :: PhoneNumberCountryCode
pattern PNCCCX = PhoneNumberCountryCode' "CX"

pattern PNCCCY :: PhoneNumberCountryCode
pattern PNCCCY = PhoneNumberCountryCode' "CY"

pattern PNCCCZ :: PhoneNumberCountryCode
pattern PNCCCZ = PhoneNumberCountryCode' "CZ"

pattern PNCCDE :: PhoneNumberCountryCode
pattern PNCCDE = PhoneNumberCountryCode' "DE"

pattern PNCCDJ :: PhoneNumberCountryCode
pattern PNCCDJ = PhoneNumberCountryCode' "DJ"

pattern PNCCDK :: PhoneNumberCountryCode
pattern PNCCDK = PhoneNumberCountryCode' "DK"

pattern PNCCDM :: PhoneNumberCountryCode
pattern PNCCDM = PhoneNumberCountryCode' "DM"

pattern PNCCDO :: PhoneNumberCountryCode
pattern PNCCDO = PhoneNumberCountryCode' "DO"

pattern PNCCDZ :: PhoneNumberCountryCode
pattern PNCCDZ = PhoneNumberCountryCode' "DZ"

pattern PNCCEC :: PhoneNumberCountryCode
pattern PNCCEC = PhoneNumberCountryCode' "EC"

pattern PNCCEE :: PhoneNumberCountryCode
pattern PNCCEE = PhoneNumberCountryCode' "EE"

pattern PNCCEG :: PhoneNumberCountryCode
pattern PNCCEG = PhoneNumberCountryCode' "EG"

pattern PNCCEH :: PhoneNumberCountryCode
pattern PNCCEH = PhoneNumberCountryCode' "EH"

pattern PNCCER :: PhoneNumberCountryCode
pattern PNCCER = PhoneNumberCountryCode' "ER"

pattern PNCCES :: PhoneNumberCountryCode
pattern PNCCES = PhoneNumberCountryCode' "ES"

pattern PNCCET :: PhoneNumberCountryCode
pattern PNCCET = PhoneNumberCountryCode' "ET"

pattern PNCCFI :: PhoneNumberCountryCode
pattern PNCCFI = PhoneNumberCountryCode' "FI"

pattern PNCCFJ :: PhoneNumberCountryCode
pattern PNCCFJ = PhoneNumberCountryCode' "FJ"

pattern PNCCFK :: PhoneNumberCountryCode
pattern PNCCFK = PhoneNumberCountryCode' "FK"

pattern PNCCFM :: PhoneNumberCountryCode
pattern PNCCFM = PhoneNumberCountryCode' "FM"

pattern PNCCFO :: PhoneNumberCountryCode
pattern PNCCFO = PhoneNumberCountryCode' "FO"

pattern PNCCFR :: PhoneNumberCountryCode
pattern PNCCFR = PhoneNumberCountryCode' "FR"

pattern PNCCGA :: PhoneNumberCountryCode
pattern PNCCGA = PhoneNumberCountryCode' "GA"

pattern PNCCGB :: PhoneNumberCountryCode
pattern PNCCGB = PhoneNumberCountryCode' "GB"

pattern PNCCGD :: PhoneNumberCountryCode
pattern PNCCGD = PhoneNumberCountryCode' "GD"

pattern PNCCGE :: PhoneNumberCountryCode
pattern PNCCGE = PhoneNumberCountryCode' "GE"

pattern PNCCGG :: PhoneNumberCountryCode
pattern PNCCGG = PhoneNumberCountryCode' "GG"

pattern PNCCGH :: PhoneNumberCountryCode
pattern PNCCGH = PhoneNumberCountryCode' "GH"

pattern PNCCGI :: PhoneNumberCountryCode
pattern PNCCGI = PhoneNumberCountryCode' "GI"

pattern PNCCGL :: PhoneNumberCountryCode
pattern PNCCGL = PhoneNumberCountryCode' "GL"

pattern PNCCGM :: PhoneNumberCountryCode
pattern PNCCGM = PhoneNumberCountryCode' "GM"

pattern PNCCGN :: PhoneNumberCountryCode
pattern PNCCGN = PhoneNumberCountryCode' "GN"

pattern PNCCGQ :: PhoneNumberCountryCode
pattern PNCCGQ = PhoneNumberCountryCode' "GQ"

pattern PNCCGR :: PhoneNumberCountryCode
pattern PNCCGR = PhoneNumberCountryCode' "GR"

pattern PNCCGT :: PhoneNumberCountryCode
pattern PNCCGT = PhoneNumberCountryCode' "GT"

pattern PNCCGU :: PhoneNumberCountryCode
pattern PNCCGU = PhoneNumberCountryCode' "GU"

pattern PNCCGW :: PhoneNumberCountryCode
pattern PNCCGW = PhoneNumberCountryCode' "GW"

pattern PNCCGY :: PhoneNumberCountryCode
pattern PNCCGY = PhoneNumberCountryCode' "GY"

pattern PNCCHK :: PhoneNumberCountryCode
pattern PNCCHK = PhoneNumberCountryCode' "HK"

pattern PNCCHN :: PhoneNumberCountryCode
pattern PNCCHN = PhoneNumberCountryCode' "HN"

pattern PNCCHR :: PhoneNumberCountryCode
pattern PNCCHR = PhoneNumberCountryCode' "HR"

pattern PNCCHT :: PhoneNumberCountryCode
pattern PNCCHT = PhoneNumberCountryCode' "HT"

pattern PNCCHU :: PhoneNumberCountryCode
pattern PNCCHU = PhoneNumberCountryCode' "HU"

pattern PNCCIE :: PhoneNumberCountryCode
pattern PNCCIE = PhoneNumberCountryCode' "IE"

pattern PNCCIL :: PhoneNumberCountryCode
pattern PNCCIL = PhoneNumberCountryCode' "IL"

pattern PNCCIM :: PhoneNumberCountryCode
pattern PNCCIM = PhoneNumberCountryCode' "IM"

pattern PNCCIN :: PhoneNumberCountryCode
pattern PNCCIN = PhoneNumberCountryCode' "IN"

pattern PNCCIO :: PhoneNumberCountryCode
pattern PNCCIO = PhoneNumberCountryCode' "IO"

pattern PNCCIQ :: PhoneNumberCountryCode
pattern PNCCIQ = PhoneNumberCountryCode' "IQ"

pattern PNCCIR :: PhoneNumberCountryCode
pattern PNCCIR = PhoneNumberCountryCode' "IR"

pattern PNCCIS :: PhoneNumberCountryCode
pattern PNCCIS = PhoneNumberCountryCode' "IS"

pattern PNCCIT :: PhoneNumberCountryCode
pattern PNCCIT = PhoneNumberCountryCode' "IT"

pattern PNCCId :: PhoneNumberCountryCode
pattern PNCCId = PhoneNumberCountryCode' "ID"

pattern PNCCJE :: PhoneNumberCountryCode
pattern PNCCJE = PhoneNumberCountryCode' "JE"

pattern PNCCJM :: PhoneNumberCountryCode
pattern PNCCJM = PhoneNumberCountryCode' "JM"

pattern PNCCJO :: PhoneNumberCountryCode
pattern PNCCJO = PhoneNumberCountryCode' "JO"

pattern PNCCJP :: PhoneNumberCountryCode
pattern PNCCJP = PhoneNumberCountryCode' "JP"

pattern PNCCKE :: PhoneNumberCountryCode
pattern PNCCKE = PhoneNumberCountryCode' "KE"

pattern PNCCKG :: PhoneNumberCountryCode
pattern PNCCKG = PhoneNumberCountryCode' "KG"

pattern PNCCKH :: PhoneNumberCountryCode
pattern PNCCKH = PhoneNumberCountryCode' "KH"

pattern PNCCKI :: PhoneNumberCountryCode
pattern PNCCKI = PhoneNumberCountryCode' "KI"

pattern PNCCKM :: PhoneNumberCountryCode
pattern PNCCKM = PhoneNumberCountryCode' "KM"

pattern PNCCKN :: PhoneNumberCountryCode
pattern PNCCKN = PhoneNumberCountryCode' "KN"

pattern PNCCKP :: PhoneNumberCountryCode
pattern PNCCKP = PhoneNumberCountryCode' "KP"

pattern PNCCKR :: PhoneNumberCountryCode
pattern PNCCKR = PhoneNumberCountryCode' "KR"

pattern PNCCKW :: PhoneNumberCountryCode
pattern PNCCKW = PhoneNumberCountryCode' "KW"

pattern PNCCKY :: PhoneNumberCountryCode
pattern PNCCKY = PhoneNumberCountryCode' "KY"

pattern PNCCKZ :: PhoneNumberCountryCode
pattern PNCCKZ = PhoneNumberCountryCode' "KZ"

pattern PNCCLA :: PhoneNumberCountryCode
pattern PNCCLA = PhoneNumberCountryCode' "LA"

pattern PNCCLB :: PhoneNumberCountryCode
pattern PNCCLB = PhoneNumberCountryCode' "LB"

pattern PNCCLC :: PhoneNumberCountryCode
pattern PNCCLC = PhoneNumberCountryCode' "LC"

pattern PNCCLI :: PhoneNumberCountryCode
pattern PNCCLI = PhoneNumberCountryCode' "LI"

pattern PNCCLK :: PhoneNumberCountryCode
pattern PNCCLK = PhoneNumberCountryCode' "LK"

pattern PNCCLR :: PhoneNumberCountryCode
pattern PNCCLR = PhoneNumberCountryCode' "LR"

pattern PNCCLS :: PhoneNumberCountryCode
pattern PNCCLS = PhoneNumberCountryCode' "LS"

pattern PNCCLT :: PhoneNumberCountryCode
pattern PNCCLT = PhoneNumberCountryCode' "LT"

pattern PNCCLU :: PhoneNumberCountryCode
pattern PNCCLU = PhoneNumberCountryCode' "LU"

pattern PNCCLV :: PhoneNumberCountryCode
pattern PNCCLV = PhoneNumberCountryCode' "LV"

pattern PNCCLY :: PhoneNumberCountryCode
pattern PNCCLY = PhoneNumberCountryCode' "LY"

pattern PNCCMA :: PhoneNumberCountryCode
pattern PNCCMA = PhoneNumberCountryCode' "MA"

pattern PNCCMC :: PhoneNumberCountryCode
pattern PNCCMC = PhoneNumberCountryCode' "MC"

pattern PNCCMD :: PhoneNumberCountryCode
pattern PNCCMD = PhoneNumberCountryCode' "MD"

pattern PNCCME :: PhoneNumberCountryCode
pattern PNCCME = PhoneNumberCountryCode' "ME"

pattern PNCCMF :: PhoneNumberCountryCode
pattern PNCCMF = PhoneNumberCountryCode' "MF"

pattern PNCCMG :: PhoneNumberCountryCode
pattern PNCCMG = PhoneNumberCountryCode' "MG"

pattern PNCCMH :: PhoneNumberCountryCode
pattern PNCCMH = PhoneNumberCountryCode' "MH"

pattern PNCCMK :: PhoneNumberCountryCode
pattern PNCCMK = PhoneNumberCountryCode' "MK"

pattern PNCCML :: PhoneNumberCountryCode
pattern PNCCML = PhoneNumberCountryCode' "ML"

pattern PNCCMM :: PhoneNumberCountryCode
pattern PNCCMM = PhoneNumberCountryCode' "MM"

pattern PNCCMN :: PhoneNumberCountryCode
pattern PNCCMN = PhoneNumberCountryCode' "MN"

pattern PNCCMO :: PhoneNumberCountryCode
pattern PNCCMO = PhoneNumberCountryCode' "MO"

pattern PNCCMP :: PhoneNumberCountryCode
pattern PNCCMP = PhoneNumberCountryCode' "MP"

pattern PNCCMR :: PhoneNumberCountryCode
pattern PNCCMR = PhoneNumberCountryCode' "MR"

pattern PNCCMS :: PhoneNumberCountryCode
pattern PNCCMS = PhoneNumberCountryCode' "MS"

pattern PNCCMT :: PhoneNumberCountryCode
pattern PNCCMT = PhoneNumberCountryCode' "MT"

pattern PNCCMU :: PhoneNumberCountryCode
pattern PNCCMU = PhoneNumberCountryCode' "MU"

pattern PNCCMV :: PhoneNumberCountryCode
pattern PNCCMV = PhoneNumberCountryCode' "MV"

pattern PNCCMW :: PhoneNumberCountryCode
pattern PNCCMW = PhoneNumberCountryCode' "MW"

pattern PNCCMX :: PhoneNumberCountryCode
pattern PNCCMX = PhoneNumberCountryCode' "MX"

pattern PNCCMY :: PhoneNumberCountryCode
pattern PNCCMY = PhoneNumberCountryCode' "MY"

pattern PNCCMZ :: PhoneNumberCountryCode
pattern PNCCMZ = PhoneNumberCountryCode' "MZ"

pattern PNCCNA :: PhoneNumberCountryCode
pattern PNCCNA = PhoneNumberCountryCode' "NA"

pattern PNCCNC :: PhoneNumberCountryCode
pattern PNCCNC = PhoneNumberCountryCode' "NC"

pattern PNCCNE :: PhoneNumberCountryCode
pattern PNCCNE = PhoneNumberCountryCode' "NE"

pattern PNCCNG :: PhoneNumberCountryCode
pattern PNCCNG = PhoneNumberCountryCode' "NG"

pattern PNCCNI :: PhoneNumberCountryCode
pattern PNCCNI = PhoneNumberCountryCode' "NI"

pattern PNCCNL :: PhoneNumberCountryCode
pattern PNCCNL = PhoneNumberCountryCode' "NL"

pattern PNCCNO :: PhoneNumberCountryCode
pattern PNCCNO = PhoneNumberCountryCode' "NO"

pattern PNCCNP :: PhoneNumberCountryCode
pattern PNCCNP = PhoneNumberCountryCode' "NP"

pattern PNCCNR :: PhoneNumberCountryCode
pattern PNCCNR = PhoneNumberCountryCode' "NR"

pattern PNCCNU :: PhoneNumberCountryCode
pattern PNCCNU = PhoneNumberCountryCode' "NU"

pattern PNCCNZ :: PhoneNumberCountryCode
pattern PNCCNZ = PhoneNumberCountryCode' "NZ"

pattern PNCCOM :: PhoneNumberCountryCode
pattern PNCCOM = PhoneNumberCountryCode' "OM"

pattern PNCCPA :: PhoneNumberCountryCode
pattern PNCCPA = PhoneNumberCountryCode' "PA"

pattern PNCCPE :: PhoneNumberCountryCode
pattern PNCCPE = PhoneNumberCountryCode' "PE"

pattern PNCCPF :: PhoneNumberCountryCode
pattern PNCCPF = PhoneNumberCountryCode' "PF"

pattern PNCCPG :: PhoneNumberCountryCode
pattern PNCCPG = PhoneNumberCountryCode' "PG"

pattern PNCCPH :: PhoneNumberCountryCode
pattern PNCCPH = PhoneNumberCountryCode' "PH"

pattern PNCCPK :: PhoneNumberCountryCode
pattern PNCCPK = PhoneNumberCountryCode' "PK"

pattern PNCCPL :: PhoneNumberCountryCode
pattern PNCCPL = PhoneNumberCountryCode' "PL"

pattern PNCCPM :: PhoneNumberCountryCode
pattern PNCCPM = PhoneNumberCountryCode' "PM"

pattern PNCCPN :: PhoneNumberCountryCode
pattern PNCCPN = PhoneNumberCountryCode' "PN"

pattern PNCCPR :: PhoneNumberCountryCode
pattern PNCCPR = PhoneNumberCountryCode' "PR"

pattern PNCCPT :: PhoneNumberCountryCode
pattern PNCCPT = PhoneNumberCountryCode' "PT"

pattern PNCCPW :: PhoneNumberCountryCode
pattern PNCCPW = PhoneNumberCountryCode' "PW"

pattern PNCCPY :: PhoneNumberCountryCode
pattern PNCCPY = PhoneNumberCountryCode' "PY"

pattern PNCCQA :: PhoneNumberCountryCode
pattern PNCCQA = PhoneNumberCountryCode' "QA"

pattern PNCCRE :: PhoneNumberCountryCode
pattern PNCCRE = PhoneNumberCountryCode' "RE"

pattern PNCCRO :: PhoneNumberCountryCode
pattern PNCCRO = PhoneNumberCountryCode' "RO"

pattern PNCCRS :: PhoneNumberCountryCode
pattern PNCCRS = PhoneNumberCountryCode' "RS"

pattern PNCCRU :: PhoneNumberCountryCode
pattern PNCCRU = PhoneNumberCountryCode' "RU"

pattern PNCCRW :: PhoneNumberCountryCode
pattern PNCCRW = PhoneNumberCountryCode' "RW"

pattern PNCCSA :: PhoneNumberCountryCode
pattern PNCCSA = PhoneNumberCountryCode' "SA"

pattern PNCCSB :: PhoneNumberCountryCode
pattern PNCCSB = PhoneNumberCountryCode' "SB"

pattern PNCCSC :: PhoneNumberCountryCode
pattern PNCCSC = PhoneNumberCountryCode' "SC"

pattern PNCCSD :: PhoneNumberCountryCode
pattern PNCCSD = PhoneNumberCountryCode' "SD"

pattern PNCCSE :: PhoneNumberCountryCode
pattern PNCCSE = PhoneNumberCountryCode' "SE"

pattern PNCCSG :: PhoneNumberCountryCode
pattern PNCCSG = PhoneNumberCountryCode' "SG"

pattern PNCCSH :: PhoneNumberCountryCode
pattern PNCCSH = PhoneNumberCountryCode' "SH"

pattern PNCCSI :: PhoneNumberCountryCode
pattern PNCCSI = PhoneNumberCountryCode' "SI"

pattern PNCCSJ :: PhoneNumberCountryCode
pattern PNCCSJ = PhoneNumberCountryCode' "SJ"

pattern PNCCSK :: PhoneNumberCountryCode
pattern PNCCSK = PhoneNumberCountryCode' "SK"

pattern PNCCSL :: PhoneNumberCountryCode
pattern PNCCSL = PhoneNumberCountryCode' "SL"

pattern PNCCSM :: PhoneNumberCountryCode
pattern PNCCSM = PhoneNumberCountryCode' "SM"

pattern PNCCSN :: PhoneNumberCountryCode
pattern PNCCSN = PhoneNumberCountryCode' "SN"

pattern PNCCSO :: PhoneNumberCountryCode
pattern PNCCSO = PhoneNumberCountryCode' "SO"

pattern PNCCSR :: PhoneNumberCountryCode
pattern PNCCSR = PhoneNumberCountryCode' "SR"

pattern PNCCST :: PhoneNumberCountryCode
pattern PNCCST = PhoneNumberCountryCode' "ST"

pattern PNCCSV :: PhoneNumberCountryCode
pattern PNCCSV = PhoneNumberCountryCode' "SV"

pattern PNCCSX :: PhoneNumberCountryCode
pattern PNCCSX = PhoneNumberCountryCode' "SX"

pattern PNCCSY :: PhoneNumberCountryCode
pattern PNCCSY = PhoneNumberCountryCode' "SY"

pattern PNCCSZ :: PhoneNumberCountryCode
pattern PNCCSZ = PhoneNumberCountryCode' "SZ"

pattern PNCCTC :: PhoneNumberCountryCode
pattern PNCCTC = PhoneNumberCountryCode' "TC"

pattern PNCCTD :: PhoneNumberCountryCode
pattern PNCCTD = PhoneNumberCountryCode' "TD"

pattern PNCCTG :: PhoneNumberCountryCode
pattern PNCCTG = PhoneNumberCountryCode' "TG"

pattern PNCCTH :: PhoneNumberCountryCode
pattern PNCCTH = PhoneNumberCountryCode' "TH"

pattern PNCCTJ :: PhoneNumberCountryCode
pattern PNCCTJ = PhoneNumberCountryCode' "TJ"

pattern PNCCTK :: PhoneNumberCountryCode
pattern PNCCTK = PhoneNumberCountryCode' "TK"

pattern PNCCTL :: PhoneNumberCountryCode
pattern PNCCTL = PhoneNumberCountryCode' "TL"

pattern PNCCTM :: PhoneNumberCountryCode
pattern PNCCTM = PhoneNumberCountryCode' "TM"

pattern PNCCTN :: PhoneNumberCountryCode
pattern PNCCTN = PhoneNumberCountryCode' "TN"

pattern PNCCTO :: PhoneNumberCountryCode
pattern PNCCTO = PhoneNumberCountryCode' "TO"

pattern PNCCTR :: PhoneNumberCountryCode
pattern PNCCTR = PhoneNumberCountryCode' "TR"

pattern PNCCTT :: PhoneNumberCountryCode
pattern PNCCTT = PhoneNumberCountryCode' "TT"

pattern PNCCTV :: PhoneNumberCountryCode
pattern PNCCTV = PhoneNumberCountryCode' "TV"

pattern PNCCTW :: PhoneNumberCountryCode
pattern PNCCTW = PhoneNumberCountryCode' "TW"

pattern PNCCTZ :: PhoneNumberCountryCode
pattern PNCCTZ = PhoneNumberCountryCode' "TZ"

pattern PNCCUA :: PhoneNumberCountryCode
pattern PNCCUA = PhoneNumberCountryCode' "UA"

pattern PNCCUG :: PhoneNumberCountryCode
pattern PNCCUG = PhoneNumberCountryCode' "UG"

pattern PNCCUS :: PhoneNumberCountryCode
pattern PNCCUS = PhoneNumberCountryCode' "US"

pattern PNCCUY :: PhoneNumberCountryCode
pattern PNCCUY = PhoneNumberCountryCode' "UY"

pattern PNCCUZ :: PhoneNumberCountryCode
pattern PNCCUZ = PhoneNumberCountryCode' "UZ"

pattern PNCCVA :: PhoneNumberCountryCode
pattern PNCCVA = PhoneNumberCountryCode' "VA"

pattern PNCCVC :: PhoneNumberCountryCode
pattern PNCCVC = PhoneNumberCountryCode' "VC"

pattern PNCCVE :: PhoneNumberCountryCode
pattern PNCCVE = PhoneNumberCountryCode' "VE"

pattern PNCCVG :: PhoneNumberCountryCode
pattern PNCCVG = PhoneNumberCountryCode' "VG"

pattern PNCCVI :: PhoneNumberCountryCode
pattern PNCCVI = PhoneNumberCountryCode' "VI"

pattern PNCCVN :: PhoneNumberCountryCode
pattern PNCCVN = PhoneNumberCountryCode' "VN"

pattern PNCCVU :: PhoneNumberCountryCode
pattern PNCCVU = PhoneNumberCountryCode' "VU"

pattern PNCCWF :: PhoneNumberCountryCode
pattern PNCCWF = PhoneNumberCountryCode' "WF"

pattern PNCCWS :: PhoneNumberCountryCode
pattern PNCCWS = PhoneNumberCountryCode' "WS"

pattern PNCCYE :: PhoneNumberCountryCode
pattern PNCCYE = PhoneNumberCountryCode' "YE"

pattern PNCCYT :: PhoneNumberCountryCode
pattern PNCCYT = PhoneNumberCountryCode' "YT"

pattern PNCCZA :: PhoneNumberCountryCode
pattern PNCCZA = PhoneNumberCountryCode' "ZA"

pattern PNCCZM :: PhoneNumberCountryCode
pattern PNCCZM = PhoneNumberCountryCode' "ZM"

pattern PNCCZW :: PhoneNumberCountryCode
pattern PNCCZW = PhoneNumberCountryCode' "ZW"

{-# COMPLETE
  PNCCAD,
  PNCCAE,
  PNCCAF,
  PNCCAG,
  PNCCAI,
  PNCCAL,
  PNCCAM,
  PNCCAN,
  PNCCAO,
  PNCCAQ,
  PNCCAR,
  PNCCAS,
  PNCCAT,
  PNCCAU,
  PNCCAW,
  PNCCAZ,
  PNCCBA,
  PNCCBB,
  PNCCBD,
  PNCCBE,
  PNCCBF,
  PNCCBG,
  PNCCBH,
  PNCCBI,
  PNCCBJ,
  PNCCBL,
  PNCCBM,
  PNCCBN,
  PNCCBO,
  PNCCBR,
  PNCCBS,
  PNCCBT,
  PNCCBW,
  PNCCBY,
  PNCCBZ,
  PNCCCA,
  PNCCCC,
  PNCCCD,
  PNCCCF,
  PNCCCG,
  PNCCCH,
  PNCCCI,
  PNCCCK,
  PNCCCL,
  PNCCCM,
  PNCCCN,
  PNCCCO,
  PNCCCR,
  PNCCCU,
  PNCCCV,
  PNCCCW,
  PNCCCX,
  PNCCCY,
  PNCCCZ,
  PNCCDE,
  PNCCDJ,
  PNCCDK,
  PNCCDM,
  PNCCDO,
  PNCCDZ,
  PNCCEC,
  PNCCEE,
  PNCCEG,
  PNCCEH,
  PNCCER,
  PNCCES,
  PNCCET,
  PNCCFI,
  PNCCFJ,
  PNCCFK,
  PNCCFM,
  PNCCFO,
  PNCCFR,
  PNCCGA,
  PNCCGB,
  PNCCGD,
  PNCCGE,
  PNCCGG,
  PNCCGH,
  PNCCGI,
  PNCCGL,
  PNCCGM,
  PNCCGN,
  PNCCGQ,
  PNCCGR,
  PNCCGT,
  PNCCGU,
  PNCCGW,
  PNCCGY,
  PNCCHK,
  PNCCHN,
  PNCCHR,
  PNCCHT,
  PNCCHU,
  PNCCIE,
  PNCCIL,
  PNCCIM,
  PNCCIN,
  PNCCIO,
  PNCCIQ,
  PNCCIR,
  PNCCIS,
  PNCCIT,
  PNCCId,
  PNCCJE,
  PNCCJM,
  PNCCJO,
  PNCCJP,
  PNCCKE,
  PNCCKG,
  PNCCKH,
  PNCCKI,
  PNCCKM,
  PNCCKN,
  PNCCKP,
  PNCCKR,
  PNCCKW,
  PNCCKY,
  PNCCKZ,
  PNCCLA,
  PNCCLB,
  PNCCLC,
  PNCCLI,
  PNCCLK,
  PNCCLR,
  PNCCLS,
  PNCCLT,
  PNCCLU,
  PNCCLV,
  PNCCLY,
  PNCCMA,
  PNCCMC,
  PNCCMD,
  PNCCME,
  PNCCMF,
  PNCCMG,
  PNCCMH,
  PNCCMK,
  PNCCML,
  PNCCMM,
  PNCCMN,
  PNCCMO,
  PNCCMP,
  PNCCMR,
  PNCCMS,
  PNCCMT,
  PNCCMU,
  PNCCMV,
  PNCCMW,
  PNCCMX,
  PNCCMY,
  PNCCMZ,
  PNCCNA,
  PNCCNC,
  PNCCNE,
  PNCCNG,
  PNCCNI,
  PNCCNL,
  PNCCNO,
  PNCCNP,
  PNCCNR,
  PNCCNU,
  PNCCNZ,
  PNCCOM,
  PNCCPA,
  PNCCPE,
  PNCCPF,
  PNCCPG,
  PNCCPH,
  PNCCPK,
  PNCCPL,
  PNCCPM,
  PNCCPN,
  PNCCPR,
  PNCCPT,
  PNCCPW,
  PNCCPY,
  PNCCQA,
  PNCCRE,
  PNCCRO,
  PNCCRS,
  PNCCRU,
  PNCCRW,
  PNCCSA,
  PNCCSB,
  PNCCSC,
  PNCCSD,
  PNCCSE,
  PNCCSG,
  PNCCSH,
  PNCCSI,
  PNCCSJ,
  PNCCSK,
  PNCCSL,
  PNCCSM,
  PNCCSN,
  PNCCSO,
  PNCCSR,
  PNCCST,
  PNCCSV,
  PNCCSX,
  PNCCSY,
  PNCCSZ,
  PNCCTC,
  PNCCTD,
  PNCCTG,
  PNCCTH,
  PNCCTJ,
  PNCCTK,
  PNCCTL,
  PNCCTM,
  PNCCTN,
  PNCCTO,
  PNCCTR,
  PNCCTT,
  PNCCTV,
  PNCCTW,
  PNCCTZ,
  PNCCUA,
  PNCCUG,
  PNCCUS,
  PNCCUY,
  PNCCUZ,
  PNCCVA,
  PNCCVC,
  PNCCVE,
  PNCCVG,
  PNCCVI,
  PNCCVN,
  PNCCVU,
  PNCCWF,
  PNCCWS,
  PNCCYE,
  PNCCYT,
  PNCCZA,
  PNCCZM,
  PNCCZW,
  PhoneNumberCountryCode'
  #-}
